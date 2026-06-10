pub struct RingBuffer<T> {
    head: usize,
    length: usize,
    data: Vec<T>,
}

impl<T> RingBuffer<T>
where
    T: Clone + Default + std::fmt::Debug,
{
    pub fn new(capacity: usize) -> Self {
        assert_ne!(capacity, 0);
        Self {
            head: 0,
            length: 0,
            data: vec![T::default(); capacity],
        }
    }

    pub fn clear(&mut self) {
        self.head = 0;
        self.length = 0;
        self.data.fill(T::default());
    }

    pub fn push_back(&mut self, value: T) {
        self.data[self.head] = value;
        self.head += 1;
        self.head %= self.data.len();
        self.length = (self.length + 1).min(self.data.len());
    }

    /// Returns an iterator starting from the most recently pushed element
    /// all the way to the oldest pushed element.
    pub fn iter(&self) -> RingBufferIter<'_, T> {
        RingBufferIter {
            head: 0,
            ring_buffer: self,
        }
    }
}

pub struct RingBufferIter<'a, T> {
    head: usize,
    ring_buffer: &'a RingBuffer<T>,
}

impl<'a, T> Iterator for RingBufferIter<'a, T>
where
    T: Clone + Default,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.head >= self.ring_buffer.length {
            return None;
        }

        let rb_prev_head = match self.ring_buffer.head.checked_sub(1) {
            Some(i) => i,
            None => self.ring_buffer.length - 1,
        };

        let read_index = match rb_prev_head.checked_sub(self.head) {
            Some(i) => i,
            None => self.ring_buffer.length - self.head,
        };

        self.head += 1;
        Some(&self.ring_buffer.data[read_index])
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ringbuffer() {
        let mut ring_buffer: RingBuffer<u32> = RingBuffer::new(5);
        for n in 0..5 {
            ring_buffer.push_back(n);
        }

        let mut rb_iter = ring_buffer.iter();
        assert_eq!(rb_iter.next(), Some(&4));
        assert_eq!(rb_iter.next(), Some(&3));
        assert_eq!(rb_iter.next(), Some(&2));
        assert_eq!(rb_iter.next(), Some(&1));
        assert_eq!(rb_iter.next(), Some(&0));
        assert_eq!(rb_iter.next(), None);

        ring_buffer.clear();
        assert_eq!(ring_buffer.iter().next(), None);
        assert_eq!(ring_buffer.head, 0);
        assert_eq!(ring_buffer.length, 0);

        // pushing more elements than ringbuffer capacity
        // should overwrite oldest element.
        for n in 0..6 {
            ring_buffer.push_back(n);
        }
        let mut rb_iter = ring_buffer.iter();
        assert_eq!(rb_iter.next(), Some(&5));
        assert_eq!(rb_iter.next(), Some(&4));
        assert_eq!(rb_iter.next(), Some(&3));
        assert_eq!(rb_iter.next(), Some(&2));
        assert_eq!(rb_iter.next(), Some(&1));
        assert_eq!(rb_iter.next(), None);

        let mut n = 5;
        for num in ring_buffer.iter() {
            assert_eq!(*num, n);
            n -= 1;
        }

        let mut ring_buffer: RingBuffer<String> = RingBuffer::new(5);
        ring_buffer.push_back(String::from("a"));
        ring_buffer.push_back(String::from("ab"));
        ring_buffer.push_back(String::from("abc"));
        ring_buffer.push_back(String::from("abcd"));
        ring_buffer.push_back(String::from("abcde"));
        ring_buffer.push_back(String::from("abcdef"));

        let mut rb_iter = ring_buffer.iter();
        assert_eq!(rb_iter.next().as_ref().map(|s| s.as_str()), Some("abcdef"));
        assert_eq!(rb_iter.next().as_ref().map(|s| s.as_str()), Some("abcde"));
        assert_eq!(rb_iter.next().as_ref().map(|s| s.as_str()), Some("abcd"));
        assert_eq!(rb_iter.next().as_ref().map(|s| s.as_str()), Some("abc"));
        assert_eq!(rb_iter.next().as_ref().map(|s| s.as_str()), Some("ab"));
        assert_eq!(rb_iter.next().as_ref().map(|s| s.as_str()), None);
    }
}
