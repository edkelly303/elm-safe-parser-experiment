#!/usr/bin/env node

const fs = require('fs');
const readline = require('node:readline');
const rl = readline.createInterface({ input: process.stdin, output: process.stdout });
const path = require('path');
const { execSync } = require("child_process");
const files = fs.readdirSync(__dirname);
const elmFiles = files.filter(file => file.endsWith(".elm"));

elmFiles.forEach(elmFile => {
  const outputFilename = path.join(__dirname, `${elmFile.slice(0, -4)}.txt`);
  const expectedOutput = getExpectedOutput(outputFilename);

  try {
    const output = execSync(`elm make ./${elmFile}`, { encoding: "utf8", stdio: 'pipe', cwd: __dirname }).toString();
    console.log(`⚠️  ${elmFile} compiled, though it shouldn't have!`);
    process.exit(1);
  }
  catch (error) {
    if (error.stderr !== expectedOutput) {
      console.log(`⚠️  ${elmFile} failed to compile, but with the wrong error message!`);
      console.log(`EXPECTED:\n\n${expectedOutput}\n\n`);
      console.log(`GOT:\n\n${error.stderr}`);

      rl.question(`*** Do you want to keep the new error? (y/n) ***\n`, answer => {
        {
          if (answer == 'y') {
            console.log(`Saving output as ${outputFilename}`);
            fs.writeFileSync(outputFilename, error.stderr);
            rl.close();
            process.exit(1);
          }
        };
      });
    }
  }
  console.log(`✅ ${elmFile}`)
}
);

function getExpectedOutput(outputFilename) {
  try {
    return fs.readFileSync(outputFilename, 'utf8');
  }
  catch (error) {
    fs.writeFileSync(outputFilename, ``);
    return ``;
  }
}