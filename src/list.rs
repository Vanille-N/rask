// List struct
use std::rc::Rc;
// use std::iter::FromIterator;

#[derive(Default, Clone)]
pub struct List<T> {
    head: Link<T>,
}

struct Elem<T> {
    item: T,
    next: Link<T>,
}

type Link<T> = Option<Rc<Elem<T>>>;

impl<T> List<T> {
    pub fn new() -> Self {
        Self { head: None }
    }

    pub fn head(&self) -> Option<&T> {
        self.head.as_ref().map(|elem| &elem.item)
    }

    pub fn tail(&self) -> Self {
        Self {
            head: self.head.as_ref().unwrap().next.clone(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn cons(&self, elem: T) -> Self {
        Self {
            head: Some(Rc::new(Elem {
                item: elem,
                next: self.head.clone(),
            })),
        }
    }

    pub fn from(v: Vec<T>) -> Self {
        let mut lst = Self::new();
        for x in v.into_iter().rev() {
            lst = lst.cons(x);
        }
        lst
    }

    pub fn len(&self) -> usize {
        self.iter().count()
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut head = self.head.take();
        while let Some(node) = head {
            if let Ok(mut node) = Rc::try_unwrap(node) {
                head = node.next.take();
            } else {
                break;
            }
        }
    }
}

pub struct Iter<'a, T> {
    next: Option<&'a Elem<T>>,
}

impl<T> List<T> {
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            next: self.head.as_deref(),
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|node| {
            self.next = node.next.as_deref();
            &node.item
        })
    }
}
//
// impl<T> FromIterator<T> for List<T> {
//     fn from_iter<I: IntoIterator<Item=T>>(iter: I) -> Self {
//         let mut lst = List::new();
//         for i in iter.into_iter().rev() {
//             lst = lst.cons(i);
//         }
//         lst
//     }
// }
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn new_list_is_empty() {
        let lst = List::<isize>::new();
        assert!(lst.head().is_none());
    }

    #[test]
    #[should_panic]
    fn new_list_has_no_tail() {
        let lst = List::<isize>::new();
        let _tl = lst.tail();
    }

    #[test]
    fn cons_adds_new_value() {
        let a = List::new();
        let b = a.cons(1usize);
        let c = a.cons(2);
        let d = c.cons(3);
        assert!(a.head().is_none());
        assert_eq!(b.head(), Some(&1));
        assert_eq!(c.head(), Some(&2));
        assert_eq!(d.head(), Some(&3));
    }

    #[test]
    fn can_access_deep_values() {
        let lst = List::new().cons(1usize).cons(2).cons(3).cons(4).cons(5);
        let bis = lst.tail().tail().tail().cons(6).cons(7);
        assert_eq!(bis.head(), Some(&7));
        assert_eq!(bis.tail().head(), Some(&6));
        assert_eq!(bis.tail().tail().head(), Some(&2));
        assert_eq!(lst.head(), Some(&5));
        assert_eq!(lst.tail().tail().tail().tail().head(), Some(&1));
        assert_eq!(lst.tail().tail().tail().tail().tail().head(), None);
    }

    #[test]
    fn iterator_test() {
        let lst = List::new().cons('c').cons('b').cons('a');
        let mut it = lst.iter();
        assert_eq!(it.next(), Some(&'a'));
        assert_eq!(it.next(), Some(&'b'));
        assert_eq!(it.next(), Some(&'c'));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn from_vec() {
        let lst = List::from(vec!['a', 'b', 'c']);
        let mut it = lst.iter();
        assert_eq!(it.next(), Some(&'a'));
        assert_eq!(it.next(), Some(&'b'));
        assert_eq!(it.next(), Some(&'c'));
        assert_eq!(it.next(), None);
        assert_eq!(lst.head(), Some(&'a'));
    }

    // #[test]
    // fn from_iter() {
    //     let lst: List<usize> = vec![0, 1, 2].iter().collect();
    //     assert_eq!(lst.head(), Some(&0));
    //     assert_eq!(lst.tail().head(), Some(&1));
    //     assert_eq!(lst.tail().tail().head(), Some(&2));
    //     assert_eq!(lst.tail().tail().tail().head(), None);
    // }
}
