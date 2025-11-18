use std::{
    sync::mpsc::{
        self, Iter, Receiver, RecvTimeoutError, SendError, Sender, TryIter, TryRecvError,
    },
    time::Duration,
};

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

impl<T, R> TwoWayChannel<T, R> {
    pub fn send(&self, t: T) -> Result<(), SendError<T>> {
        self.tx.send(t)
    }
}

impl<T, R> TwoWayChannel<T, R> {
    pub fn iter(&self) -> Iter<'_, R> {
        self.rx.iter()
    }

    pub fn recv(&self) -> Result<R, mpsc::RecvError> {
        self.rx.recv()
    }

    pub fn recv_timeout(&self, timeout: Duration) -> Result<R, RecvTimeoutError> {
        self.rx.recv_timeout(timeout)
    }

    pub fn try_iter(&self) -> TryIter<'_, R> {
        self.rx.try_iter()
    }

    pub fn try_recv(&self) -> Result<R, TryRecvError> {
        self.rx.try_recv()
    }
}
