// List struct
use std::rc::Rc;

pub struct List<T> {
    head: Option<Elem<T>>,
}

struct Elem<T> {
    item: T,
    next: Link<T>,
}

type Link<T> = Rc<Elem<T>>;

impl<T> List<T> {
    pub fn new() -> Self {
        Self { head: None }
    }

    pub fn head(&self) -> Option<&T> {
        self.head.as_ref().map(|elem| &elem.item)
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
}
