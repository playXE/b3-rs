/*#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct BitMap<const BITMAP_SIZE: usize>
where
    [(); (BITMAP_SIZE + 31) / 32]:,
{
    bits: [u32; (BITMAP_SIZE + 31) / 32],
}

impl<const BITMAP_SIZE: usize> BitMap<BITMAP_SIZE>
where
    [(); (BITMAP_SIZE + 31) / 32]:,
{
    pub const WORD_SIZE: usize = 32;
    pub const WORDS: usize = (BITMAP_SIZE + Self::WORD_SIZE - 1) / Self::WORD_SIZE;

    pub const fn new() -> Self {
        Self {
            bits: [0; (BITMAP_SIZE + 31) / 32],
        }
    }

    pub const fn get(&self, n: usize) -> bool {
        (self.bits[n / Self::WORD_SIZE] & (1 << (n % Self::WORD_SIZE))) != 0
    }

    pub fn set(&mut self, n: usize) {
        self.bits[n / Self::WORD_SIZE] |= 1 << (n % Self::WORD_SIZE);
    }

    pub fn clear(&mut self, n: usize) {
        self.bits[n / Self::WORD_SIZE] &= !(1 << (n % Self::WORD_SIZE));
    }

    pub fn count(&self) -> usize {
        let mut count = 0;
        for i in 0..Self::WORDS {
            count += self.bits[i].count_ones() as usize;
        }
        count
    }

    pub fn set_value(&mut self, n: usize, value: bool) {
        if value {
            self.set(n);
        } else {
            self.clear(n);
        }
    }

    pub fn test_and_set(&mut self, n: usize) -> bool {
        let word = n / Self::WORD_SIZE;
        let bit = n % Self::WORD_SIZE;
        let old = self.bits[word];
        self.bits[word] |= 1 << bit;
        (old & (1 << bit)) != 0
    }

    pub fn test_and_clear(&mut self, n: usize) -> bool {
        let word = n / Self::WORD_SIZE;
        let bit = n % Self::WORD_SIZE;
        let old = self.bits[word];
        self.bits[word] &= !(1 << bit);
        (old & (1 << bit)) != 0
    }

    pub fn clear_all(&mut self) {
        for i in 0..Self::WORDS {
            self.bits[i] = 0;
        }
    }

    pub fn cleanse_last_word(&mut self) {
        if (BITMAP_SIZE % Self::WORD_SIZE) != 0 {
            let remaining_bits = BITMAP_SIZE % Self::WORD_SIZE;
            let mask = (1 << remaining_bits) - 1;
            self.bits[Self::WORDS - 1] &= mask;
        }
    }
    pub fn set_all(&mut self) {
        for i in 0..Self::WORDS {
            self.bits[i] = u32::max_value();
        }
        self.cleanse_last_word();
    }

    pub fn invert(&mut self) {
        for i in 0..Self::WORDS {
            self.bits[i] = !self.bits[i];
        }
        self.cleanse_last_word();
    }

    pub fn filter(&mut self, other: &Self) {
        for i in 0..Self::WORDS {
            self.bits[i] &= other.bits[i];
        }
    }

    pub fn exclude(&mut self, other: &Self) {
        for i in 0..Self::WORDS {
            self.bits[i] &= !other.bits[i];
        }
    }

    pub fn subsumes(&self, other: &Self) -> bool {
        for i in 0..Self::WORDS {
            let my_bits = self.bits[i];
            let other_bits = other.bits[i];

            if (my_bits | other_bits) != my_bits {
                return false;
            }
        }
        true
    }

    pub fn for_each_set_bit<F: FnMut(usize) -> bool>(&self, mut f: F) {
        for i in 0..Self::WORDS {
            let mut word = self.bits[i];
            if word == 0 {
                continue;
            }

            let base = i * Self::WORD_SIZE;

            for j in 0..Self::WORD_SIZE {
                if (word & 1) != 0 {
                    if f(base + j) {
                        return;
                    }
                }

                word >>= 1;
            }
        }
    }

    pub fn find_bit(&mut self, start_index: usize, value: bool) -> Option<usize> {
        let mut word = start_index / Self::WORD_SIZE;
        let mut bit = start_index % Self::WORD_SIZE;

        while word < Self::WORDS {
            let mut current_word = self.bits[word];
            if bit != 0 {
                current_word >>= bit;
            }

            while current_word != 0 {
                if (current_word & 1) == (value as u32) {
                    return Some(word * Self::WORD_SIZE + bit);
                }

                current_word >>= 1;
                bit += 1;
            }

            word += 1;
            bit = 0;
        }

        None
    }

    pub fn merge(&mut self, other: &Self) {
        for i in 0..Self::WORDS {
            self.bits[i] |= other.bits[i];
        }
    }

    pub fn len(&self) -> usize {
        BITMAP_SIZE
    }
}

impl<const N: usize> std::fmt::Display for BitMap<N>
where
    [(); (N + 31) / 32]:,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.len() {
            write!(f, "{}", if self.get(i) { "1" } else { "-" })?;
        }
        Ok(())
    }
}
*/

#[macro_export]
macro_rules! bitmap {
    ($name: ident, $size: expr) => {
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Default)]
        pub struct $name
        {
            bits: [u32; ($size + 31) / 32],
        }

        impl $name
        {
            pub const WORD_SIZE: usize = 32;
            pub const WORDS: usize = ($size + Self::WORD_SIZE - 1) / Self::WORD_SIZE;

            pub const fn new() -> Self {
                Self {
                    bits: [0; ($size + 31) / 32],
                }
            }

            pub const fn get(&self, n: usize) -> bool {
                (self.bits[n / Self::WORD_SIZE] & (1 << (n % Self::WORD_SIZE))) != 0
            }

            pub fn set(&mut self, n: usize) {
                self.bits[n / Self::WORD_SIZE] |= 1 << (n % Self::WORD_SIZE);
            }

            pub fn clear(&mut self, n: usize) {
                self.bits[n / Self::WORD_SIZE] &= !(1 << (n % Self::WORD_SIZE));
            }

            pub fn count(&self) -> usize {
                let mut count = 0;
                for i in 0..Self::WORDS {
                    count += self.bits[i].count_ones() as usize;
                }
                count
            }

            pub fn set_value(&mut self, n: usize, value: bool) {
                if value {
                    self.set(n);
                } else {
                    self.clear(n);
                }
            }

            pub fn test_and_set(&mut self, n: usize) -> bool {
                let word = n / Self::WORD_SIZE;
                let bit = n % Self::WORD_SIZE;
                let old = self.bits[word];
                self.bits[word] |= 1 << bit;
                (old & (1 << bit)) != 0
            }

            pub fn test_and_clear(&mut self, n: usize) -> bool {
                let word = n / Self::WORD_SIZE;
                let bit = n % Self::WORD_SIZE;
                let old = self.bits[word];
                self.bits[word] &= !(1 << bit);
                (old & (1 << bit)) != 0
            }

            pub fn clear_all(&mut self) {
                for i in 0..Self::WORDS {
                    self.bits[i] = 0;
                }
            }

            pub fn cleanse_last_word(&mut self) {
                if ($size % Self::WORD_SIZE) != 0 {
                    let remaining_bits = $size % Self::WORD_SIZE;
                    let mask = (1 << remaining_bits) - 1;
                    self.bits[Self::WORDS - 1] &= mask;
                }
            }
            pub fn set_all(&mut self) {
                for i in 0..Self::WORDS {
                    self.bits[i] = u32::max_value();
                }
                self.cleanse_last_word();
            }

            pub fn invert(&mut self) {
                for i in 0..Self::WORDS {
                    self.bits[i] = !self.bits[i];
                }
                self.cleanse_last_word();
            }

            pub fn filter(&mut self, other: &Self) {
                for i in 0..Self::WORDS {
                    self.bits[i] &= other.bits[i];
                }
            }

            pub fn exclude(&mut self, other: &Self) {
                for i in 0..Self::WORDS {
                    self.bits[i] &= !other.bits[i];
                }
            }

            pub fn subsumes(&self, other: &Self) -> bool {
                for i in 0..Self::WORDS {
                    let my_bits = self.bits[i];
                    let other_bits = other.bits[i];

                    if (my_bits | other_bits) != my_bits {
                        return false;
                    }
                }
                true
            }

            pub fn for_each_set_bit<F: FnMut(usize) -> bool>(&self, mut f: F) {
                for i in 0..Self::WORDS {
                    let mut word = self.bits[i];
                    if word == 0 {
                        continue;
                    }

                    let base = i * Self::WORD_SIZE;

                    for j in 0..Self::WORD_SIZE {
                        if (word & 1) != 0 {
                            if f(base + j) {
                                return;
                            }
                        }

                        word >>= 1;
                    }
                }
            }

            pub fn find_bit(&mut self, start_index: usize, value: bool) -> Option<usize> {
                let mut word = start_index / Self::WORD_SIZE;
                let mut bit = start_index % Self::WORD_SIZE;

                while word < Self::WORDS {
                    let mut current_word = self.bits[word];
                    if bit != 0 {
                        current_word >>= bit;
                    }

                    while current_word != 0 {
                        if (current_word & 1) == (value as u32) {
                            return Some(word * Self::WORD_SIZE + bit);
                        }

                        current_word >>= 1;
                        bit += 1;
                    }

                    word += 1;
                    bit = 0;
                }

                None
            }

            pub fn merge(&mut self, other: &Self) {
                for i in 0..Self::WORDS {
                    self.bits[i] |= other.bits[i];
                }
            }

            pub fn len(&self) -> usize {
                $size
            }
        }

        impl std::fmt::Display for $name
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                for i in 0..self.len() {
                    write!(f, "{}", if self.get(i) { "1" } else { "-" })?;
                }
                Ok(())
            }
        }
    }
}