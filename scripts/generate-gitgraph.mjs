// scripts/generate-gitgraph.mjs
import fs from "fs";
import { JSDOM } from "jsdom";
import { createGitgraph } from "@gitgraph/js";

// lee datos
const branchData = JSON.parse(fs.readFileSync("branch-data.json", "utf8"));

// DOM virtual
const dom = new JSDOM(`<div id="graph"></div>`);
const document = dom.window.document;
const container = document.getElementById("graph");

// crea el gráfico
const gitgraph = createGitgraph(container);

// por cada rama, “simula” commits (no es el historial real, solo conteo)
branchData.forEach(({ name, commits }) => {
  // const branch = gitgraph.branch(name);
  const b = gitgraph.branch(name);
  // for (let i = 0; i < commits; i++) {
  //   branch.commit(`Commit ${i + 1}`);
  // }
  for (let i = 0; i < commits; i++) b.commit(`Commit ${i+1}`);
});

// guarda SVG (createGitgraph escribe dentro del contenedor)
fs.mkdirSync("assets", { recursive: true });
fs.writeFileSync("assets/gitgraph.svg", container.innerHTML, "utf8");
console.log("SVG generado en assets/gitgraph.svg");
