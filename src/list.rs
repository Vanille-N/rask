// List struct
use std::rc::Rc;

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
            head: self.head.as_ref().unwrap().next.clone()
        }
    }

    pub fn cons(&self, elem: T) -> Self {
        Self {
            head: Some(Rc::new(Elem {
                item: elem,
                next: self.head.clone(),
            })),
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_new_list() {
        let _lst = List::<isize>::new();
    }

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
}
