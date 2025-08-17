const { execSync } = require("child_process");

function sh(cmd){
  return execSync(cmd, {stdio:["ignore","pipe", "ignore"]})
    .toString()
    .trim();
}

// Obtener branches remotos
const branches = sh("git branch -r --format='%(refname:short)'")
  .split("\n")
  // .toString()
  .map((s) => s.replace(/'/g,"").trim())
  // .filter(b => b.trim() && !b.includes("HEAD"));
  .filter((b) => b && !/HEAD\b/.test(b));

// Contar commits por branch
const branchData = branches.map((branch) => {
  const name = branch.replace(/^origin\//, "");
  // const commits = execSync(`git rev-list --count ${branch}`).toString().trim();
  const commits = Number(sh(`git rev-list --count ${branch}`)) || 0;
  // return { name, commits: parseInt(commits) || 0 };
  return { name, commits };
});

// console.log(JSON.stringify(branchData));
console.log(JSON.stringify(branchData, null, 2));