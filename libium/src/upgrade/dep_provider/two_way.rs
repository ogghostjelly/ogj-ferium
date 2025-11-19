use std::sync::mpsc::{self, Receiver, Sender};

pub fn channel<T, R>() -> (TwoWayChannel<T, R>, TwoWayChannel<R, T>) {
    let (tx0, rx1) = mpsc::channel();
    let (tx1, rx0) = mpsc::channel();
    (
        TwoWayChannel { tx: tx0, rx: rx0 },
        TwoWayChannel { tx: tx1, rx: rx1 },
    )
}

pub struct TwoWayChannel<T, R> {
    pub tx: Sender<T>,
    pub rx: Receiver<R>,
}
