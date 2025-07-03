use crate::{Doc, DocPtr};

/// Visit concatenated docs of the leftmost chain in reverse order.
pub fn visit_sequence_rev<'a, 'd, T>(
    mut doc: &'d Doc<'a, T>,
    visitor: &mut impl FnMut(&'d Doc<'a, T>),
) -> &'d Doc<'a, T>
where
    T: DocPtr<'a>,
{
    // Since appended documents often appear in sequence on the left side we
    // gain a slight performance increase by batching these pushes (avoiding
    // to push and directly pop `Append` documents)
    while let Doc::Append(l, r) = doc {
        visitor(r);
        doc = l;
    }
    doc
}

/// Visit deeply concatenated docs sequentially.
pub fn visit_sequence_deep<'a, 'd, T>(
    mut doc: &'d Doc<'a, T>,
    visitor: &mut impl FnMut(&'d Doc<'a, T>),
) where
    T: DocPtr<'a>,
{
    while let Doc::Append(l, r) = doc {
        visit_sequence_deep(l, visitor);
        doc = r;
    }
    visitor(doc);
}
