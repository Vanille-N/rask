// List struct

struct List<T> {
    head: Option<Elem<T>>,
}

struct Elem<T> {
    item: T,
    next: Link<T>,
}

type Link<T> = Rc<Elem<T>>; 
