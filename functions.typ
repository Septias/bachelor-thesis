#let red_700 = rgb(185, 28, 28)

// shorthand to create a simple overline and index i
#let oi(body) = $overline(body)^i$

#let type_name(name) = text(
  font: ("FreeSans",),
  size: 9pt,
  fill: rgb(10, 10, 10),
  weight: "medium",
  name,
)

#let colored_box(title: "", color: blue, content) = {
  block(breakable: false, {
    stack(
      dir: ttb,
      spacing: 0.5em,
      align(left, {
        show heading: set text(
          fill: color,
          font: "DejaVu Sans",
          weight: "medium",
          size: 10pt,
        )
        heading(title, level: 2)
      }),
      rect(stroke: 1pt + color, radius: 4pt, width: 100%, inset: 8pt, content),
    )
  })
}

#let rule_name(name) = [
  #text(
    fill: if name.starts-with("T") { yellow } else if name.starts-with("S") {
      purple
    } else if name.starts-with("C") { orange } else if name.starts-with("R") {
      blue
    } else { black },
    smallcaps(name),
  )
]

#let derive(name, prem, conclusion) = [
  #table(
    stroke: none,
    inset: (x: 0pt, y: 5pt),
    align: center,
    table.cell(align: start, rule_name(name)),
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
