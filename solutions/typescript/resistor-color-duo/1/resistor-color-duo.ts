const ALL_COLORS = [
  "black",
  "brown",
  "red",
  "orange",
  "yellow",
  "green",
  "blue",
  "violet",
  "grey",
  "white",
];

export class ResistorColor {
  private colors: string[];

  constructor(colors: string[]) {
    if (colors.length < 2) {
      throw new Error("At least two colors need to be present");
    }
    this.colors = colors.slice(0, 2);
  }

  value(): number {
    return Number(this.colors.map((c) => ALL_COLORS.indexOf(c)).join(""));
  }
}
