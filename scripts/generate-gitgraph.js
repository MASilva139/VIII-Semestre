const { Gitgraph } = require("@gitgraph/js");
const fs = require("fs");

// Leer datos de branches
const branchData = JSON.parse(fs.readFileSync("branch-data.json"));

// Crear contenedor virtual para el SVG
const container = document.createElement("div");
const gitgraph = Gitgraph(container);

// Generar gráfico
branchData.forEach((branch) => {
  const gitBranch = gitgraph.branch(branch.name);
  for (let i = 0; i < branch.commits; i++) {
    gitBranch.commit(`Commit ${i + 1}`);
  }
});

// Guardar SVG
const svg = container.innerHTML;
fs.writeFileSync("assets/gitgraph.svg", svg);