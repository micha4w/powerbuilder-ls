use std::{ops::Deref, rc::Rc};

struct OffsetRc<Owner, Field> {
    rc: Rc<Owner>,
    field_offset: usize,
    field_type: std::marker::PhantomData<Field>,
}

impl<Owner, Field> OffsetRc<Owner, Field> {
    /// SAFETY: `offset` must be the correct offset of the field within `Owner`
    unsafe fn new(rc: Rc<Owner>, offset: usize) -> Self {
        OffsetRc {
            rc,
            field_offset: offset,
            field_type: std::marker::PhantomData,
        }
    }
}

impl<Owner, Field> Deref for OffsetRc<Owner, Field> {
    type Target = Field;
    fn deref(&self) -> &Field {
        unsafe {
            let ptr = Rc::as_ptr(&self.rc) as *const u8;
            &*(ptr.add(self.field_offset) as *const Field)
        }
    }
}

macro_rules! offset_rc {
    ($rc:expr, $Owner:ty, $field:ident) => {
        unsafe { OffsetRc::new($rc, offset_of!($Owner, $field)) }
    };
}