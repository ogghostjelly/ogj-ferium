use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
    rc::Rc,
    sync::mpsc::{RecvError, SendError},
};

use super::two_way::TwoWayChannel;

/// A cache that requests data by sending keys to an [`mpsc::Sender`] and receives the data it requested through an [`mpsc::Reciever`].
pub struct Cache<K, V> {
    data: HashMap<K, Data<V>>,
    ch: TwoWayChannel<K, (K, V)>,
}

enum Data<V> {
    Pending,
    Resolved(Rc<V>),
}

impl<K, V> Cache<K, V> {
    pub fn new(ch: TwoWayChannel<K, (K, V)>) -> Self {
        Self {
            data: HashMap::new(),
            ch,
        }
    }
}

impl<K, V> Cache<K, V>
where
    K: Eq + Hash + Clone,
{
    /// Get data from the cache or request and wait for it.
    pub fn get(&mut self, key: K) -> Result<Rc<V>, Error<K>> {
        Ok(match self.data.entry(key.clone()) {
            Entry::Occupied(e) => match e.get() {
                Data::Pending => self.wait_on(key)?,
                Data::Resolved(data) => data.clone(),
            },
            Entry::Vacant(_) => {
                self.try_request(key.clone())?;
                self.wait_on(key)?
            }
        })
    }

    /// Block and wait until a specified key is recieved from the reciever channel.
    fn wait_on(&mut self, key: K) -> Result<Rc<V>, RecvError> {
        loop {
            let (k, v) = self.ch.rx.recv()?;
            if key != k {
                self.data.insert(k, Data::Resolved(Rc::new(v)));
            } else {
                let v = Rc::new(v);
                self.data.insert(k, Data::Resolved(v.clone()));
                break Ok(v);
            }
        }
    }

    /// Make a request for a certain key if there is not one already.
    pub fn try_request(&mut self, t: K) -> Result<(), Error<K>> {
        if !self.data.contains_key(&t) {
            self.data.insert(t.clone(), Data::Pending);
            self.ch.tx.send(t)?;
        }
        Ok(())
    }
}

#[derive(thiserror::Error, Debug)]
#[error(transparent)]
pub enum Error<K> {
    Send(#[from] SendError<K>),
    Recv(#[from] RecvError),
}
