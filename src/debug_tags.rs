use std::fmt;

use crate::{Doc, DocPtr, text::Text};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DebugTagRange {
    pub tag: u32,
    pub start: usize,
    pub end: usize,
}

/// Render debug IR text and collect output ranges for tagged text leaves.
///
/// The returned text is identical to `format!("{doc:#?}")`.
pub fn debug_with_tag_ranges<'a, T>(doc: &Doc<'a, T>) -> (String, Vec<DebugTagRange>)
where
    T: DocPtr<'a> + fmt::Debug,
{
    let text = format!("{doc:#?}");
    let mut tagged_tokens = Vec::new();
    collect_tagged_tokens(doc, None, &mut tagged_tokens);

    let mut cursor = 0;
    let mut ranges = Vec::new();
    for (tag, token) in tagged_tokens {
        if token.is_empty() {
            continue;
        }
        if let Some(found) = text[cursor..].find(&token) {
            let start = cursor + found;
            let end = start + token.len();
            ranges.push(DebugTagRange { tag, start, end });
            cursor = end;
        }
    }

    (text, ranges)
}

fn collect_tagged_tokens<'a, T>(
    doc: &Doc<'a, T>,
    active_tag: Option<u32>,
    out: &mut Vec<(u32, String)>,
) where
    T: DocPtr<'a>,
{
    match doc {
        Doc::Nil | Doc::Fail | Doc::HardLine | Doc::ExpandParent => {}
        Doc::Text(text) => {
            if let Some(tag) = active_tag {
                out.push((tag, format!("{text:?}")));
            }
        }
        Doc::TextWithLen(_, inner) => collect_tagged_tokens(inner, active_tag, out),
        Doc::Append(left, right) => {
            collect_tagged_tokens(left, active_tag, out);
            collect_tagged_tokens(right, active_tag, out);
        }
        Doc::LineSuffix(inner)
        | Doc::Nest(_, inner)
        | Doc::DedentToRoot(inner)
        | Doc::Align(inner)
        | Doc::Flatten(inner) => collect_tagged_tokens(inner, active_tag, out),
        Doc::Tagged(id, inner) => collect_tagged_tokens(inner, Some(*id), out),
        Doc::BreakOrFlat(break_doc, flat_doc) => match (&**break_doc, &**flat_doc) {
            (Doc::HardLine, Doc::Text(Text::Borrowed(" "))) => {}
            (Doc::HardLine, Doc::Nil) => {}
            (_, Doc::Nil) => collect_tagged_tokens(break_doc, active_tag, out),
            (Doc::Nil, _) => collect_tagged_tokens(flat_doc, active_tag, out),
            _ => {
                // Debug prints FlatOrBreak(y, x).
                collect_tagged_tokens(flat_doc, active_tag, out);
                collect_tagged_tokens(break_doc, active_tag, out);
            }
        },
        Doc::Group(inner) => match &**inner {
            Doc::BreakOrFlat(break_doc, flat_doc)
                if matches!(
                    (&**break_doc, &**flat_doc),
                    (Doc::HardLine, Doc::Text(Text::Borrowed(" ")))
                ) => {}
            Doc::BreakOrFlat(break_doc, flat_doc)
                if matches!((&**break_doc, &**flat_doc), (Doc::HardLine, Doc::Nil)) => {}
            _ => collect_tagged_tokens(inner, active_tag, out),
        },
        Doc::Union(left, right) | Doc::PartialUnion(left, right) => {
            collect_tagged_tokens(left, active_tag, out);
            collect_tagged_tokens(right, active_tag, out);
        }
        #[cfg(feature = "contextual")]
        Doc::OnColumn(_) | Doc::OnNesting(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::{Arena, DocAllocator};

    use super::debug_with_tag_ranges;

    #[test]
    fn keeps_debug_output_stable_and_collects_ranges() {
        let arena = Arena::new();
        let doc = (arena.text("alpha").tag(1) + arena.space() + arena.text("beta").tag(2))
            .group();

        let (text, ranges) = debug_with_tag_ranges(&doc);
        assert_eq!(text, format!("{doc:#?}"));
        assert_eq!(ranges.len(), 2);
        assert_eq!(ranges[0].tag, 1);
        assert_eq!(ranges[1].tag, 2);
        assert_eq!(&text[ranges[0].start..ranges[0].end], "\"alpha\"");
        assert_eq!(&text[ranges[1].start..ranges[1].end], "\"beta\"");
    }
}
