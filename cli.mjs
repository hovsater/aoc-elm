import fs from "node:fs";
import process from "node:process";
import util from "node:util";

import AoC from "./aoc.js";

let parsedArgs;

try {
  parsedArgs = util.parseArgs({
    allowPositionals: true,
    options: {
      sample: { type: "boolean", default: false },
    },
  });
} catch (err) {
  console.error(err.message);
  process.exit(1);
}

const useSampleInput = parsedArgs.values.sample;
const fileMatcher = /^src[/]Year(?<year>\d+)[/]Day(?<day>\d+)[.]elm$/;
const problems = parsedArgs.positionals.reduce((problems, filename) => {
  const match = filename.match(fileMatcher);
  if (!match) {
    console.warn(`unexpected filename ${filename}. Skipping...`);
    return problems;
  }

  const [_, year, day] = match;
  const input = `./input/${year}-${day}${useSampleInput ? ".sample" : ""}.txt`;
  if (!fs.existsSync(input)) {
    console.warn(`input file for ${filename} not found. Skipping...`);
    return problems;
  }

  return [
    ...problems,
    {
      year: parseInt(year, 10),
      day: parseInt(day, 10),
      input: fs.readFileSync(input, { encoding: "ascii" }),
    },
  ];
}, []);

const app = AoC.Elm.Main.init();

app.ports.sendSolution.subscribe(({ year, day, part_one, part_two }) => {
  console.log(
    `${year}/${day
      .toString()
      .padStart(2, 0)}:\n\tpart one: ${part_one}\n\tpart two: ${part_two}`
  );
});

problems.forEach((problem) => app.ports.requestSolve.send(problem));
