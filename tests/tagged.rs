use prettyless::{Arena, DocAllocator, Render};

struct RecordingRender {
    output: String,
    events: Vec<String>,
}

impl RecordingRender {
    fn new() -> Self {
        Self {
            output: String::new(),
            events: Vec::new(),
        }
    }
}

impl Render for RecordingRender {
    type Error = std::fmt::Error;

    fn write_str(&mut self, s: &str) -> Result<usize, Self::Error> {
        self.output.push_str(s);
        Ok(s.len())
    }

    fn on_tag_enter(&mut self, id: u32) -> Result<(), Self::Error> {
        self.events.push(format!("enter:{id}"));
        Ok(())
    }

    fn on_tag_exit(&mut self, id: u32) -> Result<(), Self::Error> {
        self.events.push(format!("exit:{id}"));
        Ok(())
    }

    fn fail_doc(&self) -> Self::Error {
        std::fmt::Error
    }
}

#[test]
fn tagged_keeps_rendered_text_unchanged() {
    let arena = Arena::new();
    let plain = arena.text("alpha") + arena.space() + arena.text("beta");
    let tagged = arena.text("alpha").tag(1) + arena.space() + arena.text("beta").tag(2);

    assert_eq!(plain.print(80).to_string(), "alpha beta");
    assert_eq!(tagged.print(80).to_string(), "alpha beta");
}

#[test]
fn tagged_is_transparent_in_debug_output() {
    let arena = Arena::new();
    let plain = arena.text("alpha") + arena.space() + arena.text("beta");
    let tagged = arena.text("alpha").tag(1) + arena.space() + arena.text("beta").tag(2);

    assert_eq!(format!("{plain:#?}"), format!("{tagged:#?}"));
}

#[test]
fn tagged_emits_balanced_callbacks() {
    let arena = Arena::new();
    let doc = arena.text("a").tag(1) + arena.text("b").tag(2) + arena.text("c").tag(3);
    let mut render = RecordingRender::new();
    doc.render_raw(80, &mut render).unwrap();

    assert_eq!(render.output, "abc");
    assert_eq!(
        render.events,
        vec![
            "enter:1".to_string(),
            "exit:1".to_string(),
            "enter:2".to_string(),
            "exit:2".to_string(),
            "enter:3".to_string(),
            "exit:3".to_string(),
        ]
    );
}
