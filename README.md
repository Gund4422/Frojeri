![Frojeri Logo](https://github.com/Gund4422/Frojeri/raw/main/Assets/Frojeri.png)
# The Frojeri Project

Welcome to **The Frojeri Project**, a full Nintendo 64 R4300i CPU emulation written in Luau for Roblox.

---

## Overview

The Frojeri Project is a complete implementation of the N64 CPU, supporting **all 161 opcodes** including:

* Load/Store Instructions (23 opcodes)
* Arithmetic Instructions (50 opcodes)
* Jump and Branch Instructions (19 opcodes)
* Special Instructions (2 opcodes)
* Exception Instructions (11 opcodes)
* COP0 (System Control Processor) Instructions (7 opcodes)
* COP1 / FPU Instructions (34 opcodes)
* Pseudo Opcodes (15 opcodes)

This project allows you to execute N64 machine-level instructions within Roblox, making it ideal for experimentation, emulation, or educational purposes.

---

## Features

* **Complete CPU**: Emulates the R4300i processor used in the Nintendo 64.
* **Floating Point Unit (FPU)**: Full support for COP1 instructions.
* **Memory and Registers**: 32 general-purpose registers, sparse memory emulation.
* **Load/Store and Arithmetic**: Handles all standard and doubleword operations.
* **Branching & Control**: Implements all jump, branch, and pseudo instructions.
* **Customizable**: Extend or hook into opcodes for modding or experimentation.

---

## Getting Started

The Frojeri Project supports two initialization workflows so it works for both **Studio developers** and **exploit users**.

### Studio Developers

1. Download `Frojeri.lua` into `ServerScriptService` in Roblox Studio.
2. Require and initialize it in a Script/ModuleScript:

```lua
local R4300i = require(game.ServerScriptService.Frojeri)
local cpu = R4300i.new()
```

### Exploit / Loadstring Initialization

> Use this only if you understand the security/privacy implications of running remote code. This method is for exploit users who prefer loading the module at runtime.

```lua
local frojeri = loadstring(game:HttpGet("https://raw.githubusercontent.com/Gund4422/Frojeri/refs/heads/main/Frojeri.lua"))()
local cpu = frojeri.new()
```

---

## Installation

1. Copy `Frojeri.lua` into your Roblox project (ServerScriptService recommended for Studio developers).

2. Create a new Script or ModuleScript.

3. Require Frojeri and initialize (see "Getting Started" above).

4. Execute instructions with:

```lua
cpu:execute(opcode, rs, rt, rd, sa, imm_or_target)
```

For FPU instructions, use the opcode name as a string:

```lua
cpu:execute("ADD", fs, ft, fd)
```

---

## Example

```lua
-- Initialize CPU (Studio)
local R4300i = require(game.ServerScriptService.Frojeri)
local cpu = R4300i.new()

-- Load immediate value
cpu.pseudo.LI(cpu, 1, 42)

-- Move value
cpu.pseudo.MOVE(cpu, 2, 1)

print(cpu.registers[2]) -- Output: 42
```

```lua
-- Initialize CPU (Exploit / Loadstring)
local frojeri = loadstring(game:HttpGet("https://raw.githubusercontent.com/Gund4422/Frojeri/refs/heads/main/Frojeri.lua"))()
local cpu = frojeri.new()
cpu.pseudo.LI(cpu, 1, 123)
print(cpu.registers[1]) -- 123
```

---

## Notes

* Total size: ~27 KB
* Total lines: 1,000~
* Fully self-contained
* Intended for educational and experimental use in Roblox
* Current Version: v7.4

---

## License

Open-source under **GPL v3**. Credits to **Anarko** for original N64 opcode mapping.
