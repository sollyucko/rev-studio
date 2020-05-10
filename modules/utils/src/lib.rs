use std::marker::PhantomData;

pub struct IterOwnedRef<T: AsRef<U>, U: AsRef<[V]>, V> {
    data: T,
    idx: usize,
    phantom: PhantomData<*const (U, V)>,
}
impl<T: AsRef<U>, U: AsRef<[V]>, V> IterOwnedRef<T, U, V> {
    pub fn new(data: T) -> Self {
        Self {
            data,
            idx: 0,
            phantom: PhantomData,
        }
    }
}
impl<T: AsRef<U>, U: AsRef<[V]>, V> Iterator for IterOwnedRef<T, U, V>
where
    V: Clone,
{
    type Item = V;

    fn next(&mut self) -> Option<V> {
        if let Some(result) = self.data.as_ref().as_ref().get(self.idx) {
            self.idx += 1;
            Some(result.clone())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::IterOwnedRef;
    use std::rc::Rc;

    #[test]
    fn test_rc_vec() {
        let data = Rc::new(vec![1, 2, 3]);
        let mut iter = IterOwnedRef::new(data);
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), None);
    }
}
