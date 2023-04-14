#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Base(BaseType),
    Array { ty: Box<Type> },
    // Function { args: Vec<Type>, ret: Box<Type> },
    Nullable(Box<Type>),
}

impl Type {
    pub fn array(self) -> Self {
        Type::Array { ty: Box::new(self) }
    }

    pub fn nullable(self) -> Self {
        Type::Nullable(Box::new(self))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BaseType {
    Number,
    String,
    Boolean,
    Void,
}

impl From<BaseType> for Type {
    fn from(b: BaseType) -> Self {
        Type::Base(b)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Typed<T> {
    pub ty: Type,
    pub inner: T,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedVariable {
    pub name: String,
    pub ty: Type,
}

impl TypedVariable {
    pub fn new(name: String, ty: Type) -> Self {
        Self { name, ty }
    }
}
