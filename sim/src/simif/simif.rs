pub trait SimIf {
    fn poke_clock(&mut self, value: bool);
    fn poke_reset(&mut self, value: bool);
    fn step(&mut self);
    fn eval(&mut self);
}

pub trait Pokeable<T> {
    fn poke(&mut self, value: T);
}

pub trait Peekable<T> {
    fn peek(&self) -> T;
}
