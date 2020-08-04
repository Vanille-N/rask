// List struct

pub struct List<T> {
    head: Option<Elem<T>>,
}

struct Elem<T> {
    item: T,
    next: Link<T>,
}

type Link<T> = Rc<Elem<T>>;

impl List<T> {
    pub fn new() -> Self {
        Self { head: None }
    }

    pub fn head() -> Option<&T> {
        self.head.as_ref()
    }
}


#[cfg(test)]
mod tests {
    #[test]
    fn create_new_list() {
        let _lst = List::new::<isize>();
    }

    #[test]
    #[should_panic]
    fn new_list_is_empty() {
        let lst = List::new::<isize>();
        assert(lst.head().is_none());
    }
}
