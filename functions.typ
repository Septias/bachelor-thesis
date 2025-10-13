#let red_700 = rgb(185, 28, 28)

#let type_name(name) = text(
  font: ("FreeSans",),
  size: 11pt,
  fill: rgb(10, 10, 10),
  weight: "semibold",
  name,
)

#let colored_box(title: "", color: blue, content) = {
  block(breakable: false, {
    stack(
      dir: ttb,
      spacing: 0.5em,
      align(left, {
        show heading: set text(fill: color)
        heading(title)
      }),
      rect(stroke: 2pt + color, radius: 4pt, width: 100%, inset: 8pt, content),
    )
  })
}

#let derive(name, prem, conclusion) = [
  #table(
    stroke: none,
    inset: (x: 0pt, y: 5pt),
    align: center,
    table.cell(align: start)[#text(fill: if name.starts-with("T") { blue } else if name.starts-with("S") { purple } else { black }, smallcaps(name))],
    table.cell(inset: (y: 5pt), [#prem.join("     ")]),
    table.hline(),
    table.cell(inset: (y: 10pt), [#conclusion]),
  )
]

#let pad_stack(ct) = stack(dir: ltr, spacing: 3em, ..ct)


#let typings(caption, items) = figure(align(center, grid(
  align: center,
  ..items.map(pad_stack)
)))
