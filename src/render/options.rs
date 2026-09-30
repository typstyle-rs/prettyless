/// Configuration for a render pass.
///
/// Carries everything a render call needs beyond the document itself, so new controls
/// can be added without introducing another family of methods. Pass one to any render
/// method of [`crate::Doc`].
#[derive(Clone, Copy, Debug)]
#[must_use]
pub struct RenderOptions {
    width: usize,
}

impl RenderOptions {
    /// Creates options with a target line width of `width`.
    #[inline]
    pub const fn new(width: usize) -> Self {
        Self { width }
    }

    /// Replaces the target line width, keeping the other options.
    #[inline]
    pub const fn with_width(mut self, width: usize) -> Self {
        self.width = width;
        self
    }

    /// The target line width.
    ///
    /// Layout aims to keep lines within this width, but unbreakable text and line
    /// suffixes can exceed it.
    #[inline]
    pub const fn width(self) -> usize {
        self.width
    }
}
