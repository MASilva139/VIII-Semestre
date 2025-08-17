import { execSync } from "node:child_process"

function sh(cmd){
    try {
        return execSync(cmd, {
            stdio:["ignore", "pipe", "ignore"],
            encoding: "utf8"
        }).trim()
    } catch (e) {
        return "";
    }
}

const USE_REMOTES =  true
const BRANCH_CMD = USE_REMOTES
    ? `git -c core.pager=cat --no-pager branch -r --no-color --format="%(refname:short)"`
    : `git -c core.pager=cat --no-pager branch --no-color --format="%(refname:short)"`

const branchesText = sh(BRANCH_CMD);

const rawList = branchesText ? branchesText.split(/\r?\n/) : [];

let branches = rawList
    .map((s) => s.replace(/^['"]['"]$/g,"").trim())
    .filter((b) => b && !/\bHEAD\b/.test(b));
if (USE_REMOTES) {
    branches = branches.map((b) => b.replace(/^origin\//,""));
}

const branchData = branches.map((name) => {
    const ref = USE_REMOTES ? `origin/${name}` : name;
    const countText = sh(`git -c core.pager=cat --no-pager rev-list --count ${ref}`);
    const commits = Number(countText) || 0;
    return {name, commits};
});

console.log(JSON.stringify(branchData, null, 2))