// scripts/check-deps.js
const fs = require("fs");
const path = require("path");
const { spawnSync } = require("child_process");

const lockFile = path.resolve("pnpm-lock.yaml");
const modulesFile = path.resolve("node_modules", ".modules.yaml");
let needInstall = false;

if (!fs.existsSync("node_modules")) {
  console.log("No node_modules directory. Need to install.");
  needInstall = true;
} else if (!fs.existsSync(modulesFile)) {
  console.log("No .modules.yaml found. Need to install.");
  needInstall = true;
} else {
  const lockMtime = fs.statSync(lockFile).mtime;
  const modulesMtime = fs.statSync(modulesFile).mtime;
  if (lockMtime > modulesMtime) {
    console.log("Lockfile is newer than .modules.yaml. Need to install.");
    needInstall = true;
  }
}

if (needInstall) {
  console.log("Running pnpm -r install ...");
  const result = spawnSync("pnpm", ["-r", "install"], { stdio: "inherit" });
  process.exit(result.status);
} else {
  console.log("Dependencies are up to date.");
  process.exit(0);
}
