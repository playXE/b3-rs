use std::collections::VecDeque;

pub trait VecDequeExt<T> {
    fn take_first(&mut self, func: impl FnMut(&T) -> bool) -> Option<T>;
    fn take_last(&mut self, func: impl FnMut(&T) -> bool) -> Option<T>;

    fn remove_first_matching(&mut self, func: impl FnMut(&T) -> bool) -> Option<T> {
        self.take_first(func)
    }

    fn remove_all_matching(&mut self, mut func: impl FnMut(&T) -> bool) -> Vec<T> {
        let mut result = Vec::new();

        while let Some(item) = self.take_first(|item| func(item)) {
            result.push(item);
        }

        result
    }

    /// This is a priority enqueue. The callback is given a value, and if it returns true, then this
    /// will put the appended value before that value. It will keep bubbling until the callback returns
    /// false or the value ends up at the head of the queue.
    fn append_and_bubble(&mut self, value: T, func: impl FnMut(&T) -> bool);
}

impl<T> VecDequeExt<T> for VecDeque<T> {
    fn take_first(&mut self, mut func: impl FnMut(&T) -> bool) -> Option<T> {
        let mut count = 0;
        let len = self.len();

        while count < len {
            let candidate = self.pop_front().unwrap();

            if func(&candidate) {
                while count > 0 {
                    count -= 1;
                    let item = self.pop_back().unwrap();
                    self.push_front(item);
                }

                return Some(candidate);
            }

            count += 1;
            self.push_back(candidate);
        }

        None
    }
    
    fn take_last(&mut self, mut func: impl FnMut(&T) -> bool) -> Option<T> {
        let mut count = 0;
        let len = self.len();

        while count < len {
            let candidate = self.pop_back().unwrap();

            if func(&candidate) {
                while count > 0 {
                    count -= 1;
                    let item = self.pop_front().unwrap();
                    self.push_back(item);
                }

                return Some(candidate);
            }

            count += 1;
            self.push_front(candidate);
        }

        None
    }

    fn remove_first_matching(&mut self, mut func: impl FnMut(&T) -> bool) -> Option<T> {
        let mut position = 0;
        
        for item in self.iter() {
            if func(item) {
                return self.remove(position);
            }
            
            position += 1;
        }

        None
    }

    fn remove_all_matching(&mut self, mut func: impl FnMut(&T) -> bool) -> Vec<T> {
        let mut result = Vec::new();
        let mut position = 0;

        while position < self.len() {
            if func(&self[position]) {
                result.push(self.remove(position).unwrap());
            } else {
                position += 1;
            }
        }

        result
    }   
    /// This is a priority enqueue. The callback is given a value, and if it returns true, then this
    /// will put the appended value before that value. It will keep bubbling until the callback returns
    /// false or the value ends up at the head of the queue.
    fn append_and_bubble(&mut self, value: T, mut func: impl FnMut(&T) -> bool) {
        self.push_back(value);
        let mut position = self.len() - 1;

        while position > 0 {
            let prev = position;
            position -= 1;

            if !func(&self[position]) {
                return;
            }

            self.swap(position, prev);
            //std::mem::swap(&mut self[position], &mut self[prev]);
            position = prev;
        }
    }
}