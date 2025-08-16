const { execSync } = require("child_process");

// Obtener branches remotos
const branches = execSync("git branch -r --format='%(refname:short)'")
  .toString()
  .split("\n")
  .filter(b => b.trim() && !b.includes("HEAD"));

// Contar commits por branch
const branchData = branches.map((branch) => {
  const name = branch.replace("origin/", "");
  const commits = execSync(`git rev-list --count ${branch}`).toString().trim();
  return { name, commits: parseInt(commits) || 0 };
});

console.log(JSON.stringify(branchData));