use std::borrow::Cow;

#[derive(Debug)]
pub(crate) struct DropBomb {
    msg: Cow<'static, str>,
    defused: bool,
}

impl DropBomb {
    pub(crate) fn new(msg: impl Into<Cow<'static, str>>) -> DropBomb {
        DropBomb {
            msg: msg.into(),
            defused: false,
        }
    }

    pub(crate) fn defuse(&mut self) {
        self.defused = true
    }

    pub(crate) fn set_defused(&mut self, defused: bool) {
        self.defused = defused
    }

    pub(crate) fn is_defused(&self) -> bool {
        self.defused
    }
}

impl Drop for DropBomb {
    fn drop(&mut self) {
        if !self.defused && !::std::thread::panicking() {
            panic!("{}", self.msg)
        }
    }
}
