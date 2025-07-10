use prettyless::*;

#[test]
fn stress_append_left_assoc() {
    let arena = Arena::new();
    let mut doc = arena.nil();
    for _ in 0..100000 {
        doc = doc.append("a");
    }
    let mut s = String::new();
    doc.render_fmt(80, &mut s).unwrap();
}

#[test]
fn stress_append_right_assoc() {
    let arena = Arena::new();
    let mut doc = arena.nil().into_doc();
    for _ in 0..100000 {
        doc = arena.text("a").append(doc).into_doc();
    }
    let mut s = String::new();
    doc.render_fmt(80, &mut s).unwrap();
}
