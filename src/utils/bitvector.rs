use std::{
    alloc::Layout,
    hash::{Hash, Hasher},
};

/// This is a space-efficient, resizeable bitvector class. In the common case it
/// occupies one word, but if necessary, it will inflate this one word to point
/// to a single chunk of out-of-line allocated storage to store an arbitrary number
/// of bits.
///
/// - The bitvector remembers the bound of how many bits can be stored, but this
///   may be slightly greater (by as much as some platform-specific constant)
///   than the last argument passed to ensureSize().
///
/// - The bitvector can resize itself automatically (set, clear, get) or can be used
///   in a manual mode, which is faster (quickSet, quickClear, quickGet, ensureSize).
///
/// - Accesses `assert!` that you are within bounds.
///
/// - Bits are automatically initialized to zero.
///
/// On the other hand, this BitVector class may not be the fastest around, since
/// it does conditionals on every get/set/clear. But it is great if you need to
/// juggle a lot of variable-length BitVectors and you're worried about wasting
/// space.
pub struct BitVector {
    bits_or_pointer: usize,
}

impl Default for BitVector {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for BitVector {
    fn clone(&self) -> Self {
        
        if self.is_inline() {
            Self {
                bits_or_pointer: self.bits_or_pointer,
            }
        } else {
            let my_out_of_line_bits = self.out_of_line_bits();
            let mut result = Self::with_capacity(my_out_of_line_bits.num_bits());
            result.resize_out_of_line(my_out_of_line_bits.num_bits(), 0);
            result
                .out_of_line_bits_mut()
                .bits_mut()
                .copy_from_slice(my_out_of_line_bits.bits());
            result
        }
    }
}

impl BitVector {
    pub fn new() -> Self {
        Self {
            bits_or_pointer: Self::make_inline_bits(0),
        }
    }

    pub fn with_capacity(num_bits: usize) -> Self {
        let mut result = Self::new();
        result.ensure_size(num_bits);
        result
    }

    pub fn merge(&mut self, other: &Self) {
        if !self.is_inline() || !other.is_inline() {
            self.merge_slow(other);
            return;
        }

        self.bits_or_pointer |= other.bits_or_pointer;
    }

    pub fn filter(&mut self, other: &Self) {
        if !self.is_inline() || !other.is_inline() {
            self.filter_slow(other);
            return;
        }

        self.bits_or_pointer &= other.bits_or_pointer;
    }

    pub fn exclude(&mut self, other: &Self) {
        if !self.is_inline() || !other.is_inline() {
            self.exclude_slow(other);
            return;
        }

        self.bits_or_pointer &= !other.bits_or_pointer;
        assert!(self.is_inline());
    }

    fn exclude_slow(&mut self, other: &Self) {
        if other.is_inline() {
            assert!(!self.is_inline());
            let other_bits = Self::cleanse_inline_bits(other.bits_or_pointer);
            let my_bits = self.out_of_line_bits_mut();
            my_bits.bits_mut()[0] &= !other_bits;
            return;
        }

        if self.is_inline() {
            self.bits_or_pointer &= !other.out_of_line_bits().bits()[0];
            self.bits_or_pointer |= 1 << Self::max_inline_bits();
            assert!(self.is_inline());
            return;
        }

        self.ensure_size(other.len());

        assert!(!other.is_inline());
        assert!(!self.is_inline());

        let a = self.out_of_line_bits_mut();
        let b = other.out_of_line_bits();

        for i in (0..a.num_words().min(b.num_words())).rev() {
            a.bits_mut()[i] &= !b.bits()[i];
        }
    }

    fn merge_slow(&mut self, other: &Self) {
        if other.is_inline() {
            assert!(!self.is_inline());
            let other_bits = Self::cleanse_inline_bits(other.bits_or_pointer);
            let my_bits = self.out_of_line_bits_mut();
            my_bits.bits_mut()[0] |= other_bits;
            return;
        }

        self.ensure_size(other.len());

        assert!(!other.is_inline());
        assert!(!self.is_inline());

        let a = self.out_of_line_bits_mut();
        let b = other.out_of_line_bits();

        for i in (0..a.num_words()).rev() {
            a.bits_mut()[i] |= b.bits()[i];
        }
    }

    fn filter_slow(&mut self, other: &Self) {
        if other.is_inline() {
            assert!(!self.is_inline());
            let other_bits = Self::cleanse_inline_bits(other.bits_or_pointer);
            let my_bits = self.out_of_line_bits_mut();
            my_bits.bits_mut()[0] &= other_bits;
            return;
        }

        if self.is_inline() {
            self.bits_or_pointer &= other.out_of_line_bits().bits()[0];
            self.bits_or_pointer |= 1 << Self::max_inline_bits();
            assert!(self.is_inline());
            return;
        }

        self.ensure_size(other.len());

        assert!(!other.is_inline());
        assert!(!self.is_inline());

        let a = self.out_of_line_bits_mut();
        let b = other.out_of_line_bits();

        for i in (0..a.num_words().min(b.num_words())).rev() {
            a.bits_mut()[i] &= b.bits()[i];
        }

        for i in b.num_words()..a.num_words() {
            a.bits_mut()[i] = 0;
        }
    }

    pub fn is_empty(&self) -> bool {
        if self.is_inline() {
            Self::cleanse_inline_bits(self.bits_or_pointer) == 0
        } else {
            self.out_of_line_bits().bits().iter().all(|&x| x == 0)
        }
    }

    pub fn bit_count(&self) -> usize {
        if self.is_inline() {
            Self::cleanse_inline_bits(self.bits_or_pointer).count_ones() as usize
        } else {
            self.out_of_line_bits()
                .bits()
                .iter()
                .map(|&x| x.count_ones() as usize)
                .sum()
        }
    }

    pub fn find_bit(&self, index: usize, value: bool) -> usize {
        let result = self.find_bit_fast(index, value);

        debug_assert!(
            result == self.find_bit_simple(index, value),
            "find_bit_fast failed"
        );

        result
    }

    pub fn len(&self) -> usize {
        if self.is_inline() {
            Self::max_inline_bits()
        } else {
            self.out_of_line_bits().num_bits()
        }
    }

    pub fn quick_clear(&mut self, bit: usize) -> bool {
        assert!(bit < self.len());

        unsafe {
            let word = &mut *self.bits_mut().add(bit / Self::bits_in_pointer());
            let mask = 1 << (bit & (Self::bits_in_pointer() - 1));
            let result = (*word & mask) != 0;
            *word &= !mask;
            result
        }
    }

    pub fn quick_set(&mut self, bit: usize, value: bool) -> bool {
        assert!(bit < self.len());
        if value == false {
            return self.quick_clear(bit);
        }
        unsafe {
            let word = &mut *self.bits_mut().add(bit / Self::bits_in_pointer());
            let mask = 1 << (bit & (Self::bits_in_pointer() - 1));
            let result = (*word & mask) != 0;
            *word |= mask;
            result
        }
    }

    pub fn quick_get(&self, bit: usize) -> bool {
        assert!(bit < self.len());
        unsafe {
            (self.bits().add(bit / Self::bits_in_pointer()).read()
                & (1 << (bit & (Self::bits_in_pointer() - 1))))
                != 0
        }
    }

    pub fn get(&self, index: usize) -> bool {
        if index >= self.len() {
            return false;
        }

        self.quick_get(index)
    }

    pub fn contains(&self, index: usize) -> bool {
        self.get(index)
    }

    pub fn clear(&mut self, index: usize) -> bool {
        if index >= self.len() {
            return false;
        }

        self.quick_clear(index)
    }

    pub fn set(&mut self, index: usize, value: bool) -> bool {
        if value == false {
            return self.clear(index);
        }

        self.ensure_size(index + 1);
        self.quick_set(index, value)
    }

    pub fn ensure_size(&mut self, num_bits: usize) {
        if num_bits <= self.len() {
            return;
        }

        self.resize_out_of_line(num_bits, 0);
    }

    pub fn resize(&mut self, num_bits: usize) {
        if num_bits <= Self::max_inline_bits() {
            if self.is_inline() {
                return;
            }

            let my_out_of_line_bits = self.out_of_line_bits_mut();

            let bits_or_pointer = Self::make_inline_bits(my_out_of_line_bits.bits()[0] as usize);

            unsafe {
                OutOfLineBits::destroy(my_out_of_line_bits);
            }

            self.bits_or_pointer = bits_or_pointer;
            return;
        }

        self.resize_out_of_line(num_bits, 0);
    }

    pub fn clear_all(&mut self) {
        if self.is_inline() {
            self.bits_or_pointer = Self::make_inline_bits(0);
        } else {
            unsafe {
                core::ptr::write_bytes(
                    self.bits_mut().cast::<u8>(),
                    0,
                    self.out_of_line_bits().num_words() * std::mem::size_of::<usize>(),
                );
            }
        }
    }

    pub fn shift_right_by_multiple_of_64(&mut self, shift_in_bits: usize) {
        assert!(shift_in_bits % 64 == 0);
        assert!(8 % std::mem::size_of::<usize>() == 0);
        let shift_in_words = shift_in_bits / 64;
        let num_bits = self.len() + shift_in_bits;
        self.resize_out_of_line(num_bits, shift_in_words);
    }

    pub fn iter(&self) -> BitVectorIter<'_> {
        BitVectorIter {
            index: self.find_bit(0, true),
            bit_vector: self,
        }
    }

    fn resize_out_of_line(&mut self, num_bits: usize, shift_in_words: usize) {
        assert!(num_bits > Self::max_inline_bits());

        unsafe {
            let new_out_of_line_bits = OutOfLineBits::create(num_bits);
            let new_num_words = (*new_out_of_line_bits).num_words();

            if self.is_inline() {
                libc::memset(
                    (*new_out_of_line_bits).bits_mut().as_mut_ptr().cast(),
                    0,
                    shift_in_words * std::mem::size_of::<usize>(),
                );

                let addr = (*new_out_of_line_bits)
                    .bits_mut()
                    .as_mut_ptr()
                    .add(shift_in_words);

                addr.write(self.bits_or_pointer & !(1 << Self::max_inline_bits()));
                assert!(shift_in_words + 1 <= new_num_words);
                libc::memset(
                    (*new_out_of_line_bits)
                        .bits_mut()
                        .as_mut_ptr()
                        .add(shift_in_words + 1).cast(),
                    0,
                    (new_num_words - 1 - shift_in_words) * std::mem::size_of::<usize>(),
                );
            } else {
                if num_bits > self.len() {
                    let old_num_words = self.out_of_line_bits().num_words();
                    libc::memset(
                        (*new_out_of_line_bits)
                            .bits_mut()
                            .as_mut_ptr()
                            .cast(),
                        0,
                        shift_in_words * std::mem::size_of::<usize>(),
                    );

                    libc::memcpy(
                        (*new_out_of_line_bits)
                            .bits_mut()
                            .as_mut_ptr()
                            .add(shift_in_words)
                            .cast(),
                        self.out_of_line_bits().bits().as_ptr().cast(),
                        old_num_words * std::mem::size_of::<usize>(),
                    );

                    assert!(shift_in_words + old_num_words <= new_num_words);

                    libc::memset(
                        (*new_out_of_line_bits)
                            .bits_mut()
                            .as_mut_ptr()
                            .add(shift_in_words + old_num_words)
                            .cast(),
                        0,
                        (new_num_words - old_num_words - shift_in_words)
                            * std::mem::size_of::<usize>(),
                    );
                } else {
                    libc::memcpy(
                        (*new_out_of_line_bits)
                            .bits_mut()
                            .as_mut_ptr()
                            .cast(),
                        self.out_of_line_bits().bits().as_ptr().cast(),
                        new_num_words * std::mem::size_of::<usize>(),
                    );
                }

                OutOfLineBits::destroy(self.out_of_line_bits_mut());
            }

            self.bits_or_pointer = new_out_of_line_bits as usize >> 1;
        }
    }

    const fn bits_in_pointer() -> usize {
        std::mem::size_of::<usize>() << 3
    }

    const fn max_inline_bits() -> usize {
        Self::bits_in_pointer() - 1
    }

    const fn byte_count(bits: usize) -> usize {
        (bits + 7) >> 3
    }

    const fn make_inline_bits(bits: usize) -> usize {
        bits | (1 << Self::max_inline_bits())
    }

    const fn cleanse_inline_bits(bits: usize) -> usize {
        bits & !(1 << Self::max_inline_bits())
    }

    const fn is_inline(&self) -> bool {
        (self.bits_or_pointer >> Self::max_inline_bits()) != 0
    }

    fn out_of_line_bits(&self) -> &OutOfLineBits {
        unsafe { &*((self.bits_or_pointer << 1) as *const OutOfLineBits) }
    }

    fn out_of_line_bits_mut(&mut self) -> &mut OutOfLineBits {
        unsafe { &mut *((self.bits_or_pointer << 1) as *mut OutOfLineBits) }
    }

    fn bits(&self) -> *const usize {
        if self.is_inline() {
            &self.bits_or_pointer
        } else {
            self.out_of_line_bits().bits().as_ptr()
        }
    }

    fn bits_mut(&mut self) -> *mut usize {
        if self.is_inline() {
            &mut self.bits_or_pointer
        } else {
            self.out_of_line_bits_mut().bits_mut().as_mut_ptr()
        }
    }

    fn find_bit_fast(&self, start_index: usize, value: bool) -> usize {
        if self.is_inline() {
            let mut index = start_index;
            find_bit_in_word(
                self.bits_or_pointer,
                &mut index,
                Self::max_inline_bits(),
                value,
            );
            return index;
        }

        let bits = self.out_of_line_bits();

        // value = true: casts to 1, then xors to 0, then negates to 0.
        // value = false: casts to 0, then xors to 1, then negates to -1 (i.e. all one bits).
        let skip_value: usize = (value as usize ^ 1).wrapping_neg();

        let num_words = bits.num_words();

        let mut word_index = start_index / Self::bits_in_pointer();
        let mut start_index_in_word = start_index - word_index * Self::bits_in_pointer();

        while word_index < num_words {
            let word = bits.bits()[word_index];
            if word != skip_value {
                let mut index = start_index_in_word;
                if find_bit_in_word(word, &mut index, Self::bits_in_pointer(), value) {
                    return word_index * Self::bits_in_pointer() + index;
                }
            }

            word_index += 1;
            start_index_in_word = 0;
        }

        bits.num_bits()
    }

    fn find_bit_simple(&self, start_index: usize, value: bool) -> usize {
        let mut index = start_index;
        while index < self.len() {
            if self.get(index) == value {
                return index;
            }
            index += 1;
        }
        self.len()
    }
}

impl Drop for BitVector {
    fn drop(&mut self) {
        if !self.is_inline() {
            unsafe {
                OutOfLineBits::destroy(self.out_of_line_bits_mut())
            }
        }
    }
}

#[repr(C)]
struct OutOfLineBits {
    num_bits: usize,
    bits: [usize; 1],
}

impl OutOfLineBits {
    const fn num_bits(&self) -> usize {
        self.num_bits
    }

    const fn num_words(&self) -> usize {
        (self.num_bits + BitVector::bits_in_pointer() - 1) / BitVector::bits_in_pointer()
    }

    const fn bits(&self) -> &[usize] {
        unsafe { std::slice::from_raw_parts(self.bits.as_ptr() as *const usize, self.num_words()) }
    }

    fn bits_mut(&mut self) -> &mut [usize] {
        unsafe {
            std::slice::from_raw_parts_mut(self.bits.as_mut_ptr() as *mut usize, self.num_words())
        }
    }

    unsafe fn create(num_bits: usize) -> *mut Self {
        let num_bits = (num_bits + 7) & !7;
        let size = std::mem::size_of::<Self>() + std::mem::size_of::<usize>() * (num_bits / 64);
        
        /*let layout = Layout::from_size_align_unchecked(
            size,
            std::mem::align_of::<usize>(),
        );

        let ptr = std::alloc::alloc(layout) as *mut Self;*/
        let ptr = libc::malloc(size as libc::size_t) as *mut Self;
        ptr.write(Self {
            num_bits,
            bits: [0; 1],
        });
   
        ptr
    }

    unsafe fn destroy(this: *mut Self) {
        libc::free(this as *mut Self as *mut libc::c_void);
    }
}

pub fn find_bit_in_word(
    mut word: usize,
    start_or_result_index: &mut usize,
    end_index: usize,
    value: bool,
) -> bool {
    let bits_in_word = std::mem::size_of::<usize>() << 3;
    assert!(*start_or_result_index <= bits_in_word && end_index <= bits_in_word);

    let mut index = *start_or_result_index;
    word >>= index;

    word ^= (value as usize).wrapping_sub(1);
    index += word.trailing_zeros() as usize;

    if index < end_index {
        *start_or_result_index = index;
        true
    } else {
        *start_or_result_index = end_index;
        false
    }
}

impl Hash for BitVector {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if self.is_inline() {
            self.bits_or_pointer.hash(state);
        } else {
            self.out_of_line_bits().bits().hash(state);
        }
    }
}

impl PartialEq for BitVector {
    fn eq(&self, other: &Self) -> bool {
        if self.is_inline() {
            if other.is_inline() {
                return self.bits_or_pointer == other.bits_or_pointer;
            }

            return self.bits_or_pointer == other.out_of_line_bits().bits()[0];
        }

        if other.is_inline() {
            return self.out_of_line_bits().bits()[0] == other.bits_or_pointer;
        }

        self.out_of_line_bits().bits() == other.out_of_line_bits().bits()
    }
}

impl Eq for BitVector {}

pub struct BitVectorIter<'a> {
    bit_vector: &'a BitVector,
    index: usize,
}

impl<'a> Iterator for BitVectorIter<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.bit_vector.len() {
            return None;
        }
        let old = self.index;
        let index = self.bit_vector.find_bit_fast(self.index + 1, true);

        if index >= self.bit_vector.len() {
            self.index = self.bit_vector.len();
            Some(old)
        } else {
            self.index = index;
            Some(old)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.bit_vector.bit_count();
        (len, Some(len))
    }
}

impl<'a> ExactSizeIterator for BitVectorIter<'a> {
    fn len(&self) -> usize {
        self.bit_vector.bit_count()
    }
}
