pub use crate::combinators::{
    align, as_string, concat, dedent, dedent_to_root, expand_parent, fail, flat_alt, flatten,
    group, hard_line, indent, intersperse, line_or_nil, line_or_space, line_suffix, nest, nil,
    partial_union, repeat, soft_line_or_nil, soft_line_or_space, space, spaces, union, weak_line,
    weak_space,
};
#[cfg(feature = "contextual")]
pub use crate::combinators::{on_column, on_nesting};
pub use crate::{DocAllocator, DocBuilder, Pretty};
