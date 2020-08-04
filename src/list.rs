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

