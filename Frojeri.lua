-- The Frojeri Project (v7)
local R4300i = {}
R4300i.__index = R4300i

function R4300i.new()
    local self = setmetatable({}, R4300i)
    self.registers = {} -- 32 general purpose registers
    for i = 0, 31 do self.registers[i] = 0 end
    self.memory = {} -- sparse table for memory
    self.loadstore = {} -- load/store opcode handlers
    self.arithmetic = {} -- arithmetic
	self.jumpbranch = {} -- jumpbranch ig
	self.special = {}
	self.exceptions = {}
	self.cop0 = {}
	self.cop1 = {}
	self.pseudo = {}
	self.linkedAddress = nil -- for LL/SC
    return self
end

-- =============================
-- Helper Functions
-- =============================
local function signExtend8(value)
    return value >= 128 and value - 256 or value
end

local function signExtend16(value)
    return value >= 0x8000 and value - 0x10000 or value
end

local memReadByte = function(self, addr)
    return self.memory[addr] or 0
end

local memWriteByte = function(self, addr, value)
    self.memory[addr] = value & 0xFF
end

-- 32-bit helper
local function mask32(value) return value & 0xFFFFFFFF end

-- =============================
-- Load/Store Instructions
-- =============================
local LS = R4300i.loadstore

-- LB
LS[0x20] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    self.registers[rt] = signExtend8(memReadByte(self, addr))
end

-- LBU
LS[0x24] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    self.registers[rt] = memReadByte(self, addr)
end

-- LH
LS[0x21] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local value = (memReadByte(self, addr) << 8) | memReadByte(self, addr+1)
    self.registers[rt] = signExtend16(value)
end

-- LHU
LS[0x25] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local value = (memReadByte(self, addr) << 8) | memReadByte(self, addr+1)
    self.registers[rt] = value
end

-- LW
LS[0x23] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local value = (memReadByte(self, addr) << 24) | (memReadByte(self, addr+1) << 16) |
                  (memReadByte(self, addr+2) << 8) | memReadByte(self, addr+3)
    self.registers[rt] = value
end

-- LWU
LS[0x27] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local value = (memReadByte(self, addr) << 24) | (memReadByte(self, addr+1) << 16) |
                  (memReadByte(self, addr+2) << 8) | memReadByte(self, addr+3)
    self.registers[rt] = value & 0xFFFFFFFF
end

-- LD
LS[0x37] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local value = 0
    for i = 0, 7 do
        value = (value << 8) | memReadByte(self, addr+i)
    end
    self.registers[rt] = value
end

-- SB
LS[0x28] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    memWriteByte(self, addr, self.registers[rt])
end

-- SH
LS[0x29] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local value = self.registers[rt] & 0xFFFF
    memWriteByte(self, addr, (value >> 8) & 0xFF)
    memWriteByte(self, addr+1, value & 0xFF)
end

-- SW
LS[0x2A] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local value = self.registers[rt] & 0xFFFFFFFF
    memWriteByte(self, addr,   (value >> 24) & 0xFF)
    memWriteByte(self, addr+1, (value >> 16) & 0xFF)
    memWriteByte(self, addr+2, (value >> 8) & 0xFF)
    memWriteByte(self, addr+3, value & 0xFF)
end

-- LWL
LS[0x22] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local memByteCount = 4 - (addr % 4)
    local value = 0
    for i = 0, memByteCount-1 do
        value = (value << 8) | memReadByte(self, addr+i)
    end
    local mask = (1 << (8*(addr%4))) - 1
    self.registers[rt] = (self.registers[rt] & mask) | (value << (8*(addr%4)))
end

-- LWR
LS[0x26] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local byteCount = (addr % 4) + 1
    local value = 0
    for i = 0, byteCount-1 do
        value = (value << 8) | memReadByte(self, addr-i)
    end
    local mask = (~0 << (byteCount*8))
    self.registers[rt] = (self.registers[rt] & mask) | value
end

-- LL (Load Linked Word)
LS[0x30] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local value = (memReadByte(self, addr) << 24) | (memReadByte(self, addr+1) << 16) |
                  (memReadByte(self, addr+2) << 8) | memReadByte(self, addr+3)
    self.registers[rt] = value
    self.linkedAddress = addr
end

-- LLD (Load Linked Doubleword)
LS[0x34] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local value = 0
    for i = 0, 7 do
        value = (value << 8) | memReadByte(self, addr+i)
    end
    self.registers[rt] = value
    self.linkedAddress = addr
end

-- SC (Store Conditional Word)
LS[0x38] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    if self.linkedAddress == addr then
        local value = self.registers[rt] & 0xFFFFFFFF
        memWriteByte(self, addr,   (value >> 24) & 0xFF)
        memWriteByte(self, addr+1, (value >> 16) & 0xFF)
        memWriteByte(self, addr+2, (value >> 8) & 0xFF)
        memWriteByte(self, addr+3, value & 0xFF)
        self.registers[rt] = 1
    else
        self.registers[rt] = 0
    end
    self.linkedAddress = nil
end

-- SCD (Store Conditional Doubleword)
LS[0x3C] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    if self.linkedAddress == addr then
        for i = 0, 7 do
            memWriteByte(self, addr+i, (self.registers[rt] >> (8*(7-i))) & 0xFF)
        end
        self.registers[rt] = 1
    else
        self.registers[rt] = 0
    end
    self.linkedAddress = nil
end

-- LDL (Load Doubleword Left)
LS[0x2F] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local byteCount = 8 - (addr % 8)
    local value = 0
    for i = 0, byteCount-1 do
        value = (value << 8) | memReadByte(self, addr+i)
    end
    local mask = (1 << (byteCount*8)) - 1
    self.registers[rt] = (self.registers[rt] & ~mask) | value
end

-- LDR (Load Doubleword Right)
LS[0x33] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local byteCount = (addr % 8) + 1
    local value = 0
    for i = 0, byteCount-1 do
        value = (value << 8) | memReadByte(self, addr-i)
    end
    local mask = (1 << (byteCount*8)) - 1
    self.registers[rt] = (self.registers[rt] & ~mask) | value
end

-- SDL (Store Doubleword Left)
LS[0x2E] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local byteCount = 8 - (addr % 8)
    for i = 0, byteCount-1 do
        memWriteByte(self, addr+i, (self.registers[rt] >> (8*(byteCount-1-i))) & 0xFF)
    end
end

-- SDR (Store Doubleword Right)
LS[0x3B] = function(self, rs, rt, _, _, imm)
    local addr = self.registers[rs] + signExtend16(imm)
    local byteCount = (addr % 8) + 1
    for i = 0, byteCount-1 do
        memWriteByte(self, addr-i, (self.registers[rt] >> (8*i)) & 0xFF)
    end
end

-- SYNC
LS[0x0F] = function(self)
    -- no-op in Roblox
end


-- =============================
-- Arithmetic Module (AM)
-- =============================
local AM = {}


-- ADD rd, rs, rt
AM[0x20] = function(self, rs, rt, rd)
    self.registers[rd] = self.registers[rs] + self.registers[rt]
end

-- ADDU rd, rs, rt
AM[0x21] = function(self, rs, rt, rd)
    self.registers[rd] = mask32(self.registers[rs] + self.registers[rt])
end

-- SUB rd, rs, rt
AM[0x22] = function(self, rs, rt, rd)
    self.registers[rd] = self.registers[rs] - self.registers[rt]
end

-- SUBU rd, rs, rt
AM[0x23] = function(self, rs, rt, rd)
    self.registers[rd] = mask32(self.registers[rs] - self.registers[rt])
end

-- ADDI rt, rs, imm
AM[0x08] = function(self, rs, rt, _, _, imm)
    self.registers[rt] = self.registers[rs] + imm
end

-- ADDIU rt, rs, imm
AM[0x09] = function(self, rs, rt, _, _, imm)
    self.registers[rt] = mask32(self.registers[rs] + imm)
end

-- SLT rd, rs, rt
AM[0x2A] = function(self, rs, rt, rd)
    self.registers[rd] = self.registers[rs] < self.registers[rt] and 1 or 0
end

-- SLTU rd, rs, rt
AM[0x2B] = function(self, rs, rt, rd)
    local a = mask32(self.registers[rs])
    local b = mask32(self.registers[rt])
    self.registers[rd] = a < b and 1 or 0
end

-- SLTI rt, rs, imm
AM[0x0A] = function(self, rs, rt, _, _, imm)
    self.registers[rt] = self.registers[rs] < imm and 1 or 0
end

-- SLTIU rt, rs, imm
AM[0x0B] = function(self, rs, rt, _, _, imm)
    local a = mask32(self.registers[rs])
    local b = mask32(imm)
    self.registers[rt] = a < b and 1 or 0
end

-- AND rd, rs, rt
AM[0x24] = function(self, rs, rt, rd)
    self.registers[rd] = self.registers[rs] & self.registers[rt]
end

-- OR rd, rs, rt
AM[0x25] = function(self, rs, rt, rd)
    self.registers[rd] = self.registers[rs] | self.registers[rt]
end

-- XOR rd, rs, rt
AM[0x26] = function(self, rs, rt, rd)
    self.registers[rd] = self.registers[rs] ~ self.registers[rt]
end

-- NOR rd, rs, rt
AM[0x27] = function(self, rs, rt, rd)
    self.registers[rd] = ~(self.registers[rs] | self.registers[rt])
end

-- ANDI rt, rs, imm
AM[0x0C] = function(self, rs, rt, _, _, imm)
    self.registers[rt] = self.registers[rs] & imm
end

-- ORI rt, rs, imm
AM[0x0D] = function(self, rs, rt, _, _, imm)
    self.registers[rt] = self.registers[rs] | imm
end

-- XORI rt, rs, imm
AM[0x0E] = function(self, rs, rt, _, _, imm)
    self.registers[rt] = self.registers[rs] ~ imm
end

-- LUI rt, imm
AM[0x0F] = function(self, _, rt, _, _, imm)
    self.registers[rt] = (imm << 16) & 0xFFFFFFFF
end

-- SLL rd, rt, sa
AM[0x00] = function(self, _, rt, rd, sa)
    self.registers[rd] = mask32(self.registers[rt] << sa)
end

-- SRL rd, rt, sa
AM[0x02] = function(self, _, rt, rd, sa)
    self.registers[rd] = mask32(self.registers[rt] >> sa)
end

-- SRA rd, rt, sa
AM[0x03] = function(self, _, rt, rd, sa)
    self.registers[rd] = self.registers[rt] >> sa -- arithmetic shift
end

-- SLLV rd, rt, rs
AM[0x04] = function(self, rs, rt, rd)
    local sa = self.registers[rs] & 0x1F
    self.registers[rd] = mask32(self.registers[rt] << sa)
end

-- SRLV rd, rt, rs
AM[0x06] = function(self, rs, rt, rd)
    local sa = self.registers[rs] & 0x1F
    self.registers[rd] = mask32(self.registers[rt] >> sa)
end

-- SRAV rd, rt, rs
AM[0x07] = function(self, rs, rt, rd)
    local sa = self.registers[rs] & 0x1F
    self.registers[rd] = self.registers[rt] >> sa
end

-- MULT rs, rt
AM[0x18] = function(self, rs, rt)
    local result = self.registers[rs] * self.registers[rt]
    self.HI = (result >> 32) & 0xFFFFFFFF
    self.LO = result & 0xFFFFFFFF
end

-- MULTU rs, rt
AM[0x19] = function(self, rs, rt)
    local result = mask32(self.registers[rs]) * mask32(self.registers[rt])
    self.HI = (result >> 32) & 0xFFFFFFFF
    self.LO = result & 0xFFFFFFFF
end

-- DIV rs, rt
AM[0x1A] = function(self, rs, rt)
    self.LO = math.floor(self.registers[rs] / self.registers[rt])
    self.HI = self.registers[rs] % self.registers[rt]
end

-- DIVU rs, rt
AM[0x1B] = function(self, rs, rt)
    local a = mask32(self.registers[rs])
    local b = mask32(self.registers[rt])
    self.LO = math.floor(a / b)
    self.HI = a % b
end

-- MFHI rd
AM[0x10] = function(self, _, _, rd)
    self.registers[rd] = self.HI or 0
end

-- MFLO rd
AM[0x12] = function(self, _, _, rd)
    self.registers[rd] = self.LO or 0
end

-- MTHI rs
AM[0x11] = function(self, rs)
    self.HI = self.registers[rs]
end

-- MTLO rs
AM[0x13] = function(self, rs)
    self.LO = self.registers[rs]
end

-- DMULT rs, rt
AM[0x1C] = function(self, rs, rt)
    local result = self.registers[rs] * self.registers[rt]
    self.HI = (result >> 32) & 0xFFFFFFFFFFFFFFFF
    self.LO = result & 0xFFFFFFFFFFFFFFFF
end

-- DMULTU rs, rt
AM[0x1D] = function(self, rs, rt)
    local a = mask32(self.registers[rs])
    local b = mask32(self.registers[rt])
    local result = a * b
    self.HI = (result >> 32) & 0xFFFFFFFFFFFFFFFF
    self.LO = result & 0xFFFFFFFFFFFFFFFF
end

-- DDIV rs, rt
AM[0x1E] = function(self, rs, rt)
    self.LO = math.floor(self.registers[rs] / self.registers[rt])
    self.HI = self.registers[rs] % self.registers[rt]
end

-- DDIVU rs, rt
AM[0x1F] = function(self, rs, rt)
    local a = mask32(self.registers[rs])
    local b = mask32(self.registers[rt])
    self.LO = math.floor(a / b)
    self.HI = a % b
end

-- =============================
-- Attach AM to CPU
-- =============================
R4300i.arithmetic = AM

-- =============================
-- Jump/Branch opcodes
-- =============================
local JB = {}

-- BEQ rs, rt, offset
JB[0x04] = function(self, rs, rt, _, _, offset)
    if self.registers[rs] == self.registers[rt] then
        self.PC = self.PC + (offset << 2)
    end
end

-- BNE rs, rt, offset
JB[0x05] = function(self, rs, rt, _, _, offset)
    if self.registers[rs] ~= self.registers[rt] then
        self.PC = self.PC + (offset << 2)
    end
end

-- BLEZ rs, offset
JB[0x06] = function(self, rs, _, _, _, offset)
    if self.registers[rs] <= 0 then
        self.PC = self.PC + (offset << 2)
    end
end

-- BGTZ rs, offset
JB[0x07] = function(self, rs, _, _, _, offset)
    if self.registers[rs] > 0 then
        self.PC = self.PC + (offset << 2)
    end
end

-- BLTZ rs, offset
JB[0x01] = function(self, rs, _, _, _, offset)
    if self.registers[rs] < 0 then
        self.PC = self.PC + (offset << 2)
    end
end

-- BGEZ rs, offset
JB[0x01] = function(self, rs, _, _, _, offset)
    if self.registers[rs] >= 0 then
        self.PC = self.PC + (offset << 2)
    end
end

-- J target
JB[0x02] = function(self, _, _, _, _, target)
    self.PC = (self.PC & 0xF0000000) | (target << 2)
end

-- JAL target
JB[0x03] = function(self, _, _, _, _, target)
    self.registers[31] = self.PC + 8
    self.PC = (self.PC & 0xF0000000) | (target << 2)
end

-- JR rs
JB[0x08] = function(self, rs)
    self.PC = self.registers[rs]
end

-- JALR rs, rd
JB[0x09] = function(self, rs, _, rd)
    rd = rd or 31
    self.registers[rd] = self.PC + 8
    self.PC = self.registers[rs]
end

-- BEQL (likely) rs, rt, offset
JB[0x14] = function(self, rs, rt, _, _, offset)
    if self.registers[rs] == self.registers[rt] then
        self.PC = self.PC + (offset << 2)
    else
        self.PC = self.PC + 4 -- skip delay slot
    end
end

-- BNEL (likely) rs, rt, offset
JB[0x15] = function(self, rs, rt, _, _, offset)
    if self.registers[rs] ~= self.registers[rt] then
        self.PC = self.PC + (offset << 2)
    else
        self.PC = self.PC + 4
    end
end

-- BLEZL rs, offset
JB[0x16] = function(self, rs, _, _, _, offset)
    if self.registers[rs] <= 0 then
        self.PC = self.PC + (offset << 2)
    else
        self.PC = self.PC + 4
    end
end

-- BGTZL rs, offset
JB[0x17] = function(self, rs, _, _, _, offset)
    if self.registers[rs] > 0 then
        self.PC = self.PC + (offset << 2)
    else
        self.PC = self.PC + 4
    end
end

-- BLTZL rs, offset
JB[0x10] = function(self, rs, _, _, _, offset)
    if self.registers[rs] < 0 then
        self.PC = self.PC + (offset << 2)
    else
        self.PC = self.PC + 4
    end
end

-- BGEZL rs, offset
JB[0x11] = function(self, rs, _, _, _, offset)
    if self.registers[rs] >= 0 then
        self.PC = self.PC + (offset << 2)
    else
        self.PC = self.PC + 4
    end
end

-- BGEZAL rs, offset
JB[0x11] = function(self, rs, _, _, _, offset)
    self.registers[31] = self.PC + 8
    if self.registers[rs] >= 0 then
        self.PC = self.PC + (offset << 2)
    else
        self.PC = self.PC + 4
    end
end

-- BGEZALL rs, offset
JB[0x11] = function(self, rs, _, _, _, offset)
    self.registers[31] = self.PC + 8
    if self.registers[rs] >= 0 then
        self.PC = self.PC + (offset << 2)
    else
        self.PC = self.PC + 4
    end
end

-- BLTZAL rs, offset
JB[0x10] = function(self, rs, _, _, _, offset)
    self.registers[31] = self.PC + 8
    if self.registers[rs] < 0 then
        self.PC = self.PC + (offset << 2)
    else
        self.PC = self.PC + 4
    end
end

-- BLTZALL rs, offset
JB[0x10] = function(self, rs, _, _, _, offset)
    self.registers[31] = self.PC + 8
    if self.registers[rs] < 0 then
        self.PC = self.PC + (offset << 2)
    else
        self.PC = self.PC + 4
    end
end

-- Attach to CPU
R4300i.jumpbranch = JB

-- =============================
-- Special Instructions
-- =============================
local SPECIAL = {}

-- BREAK offset
SPECIAL[0x0D] = function(self, offset)
    error("BREAK instruction at PC: "..tostring(self.PC).." offset: "..tostring(offset))
end

-- SYSCALL offset
SPECIAL[0x0C] = function(self, offset)
    error("SYSCALL instruction at PC: "..tostring(self.PC).." offset: "..tostring(offset))
end

-- Attach to CPU
R4300i.special = SPECIAL

-- =============================
-- Exception / Trap Instructions
-- =============================
local EXCEPT = {}

-- TEQ rs, rt
EXCEPT[0x34] = function(self, rs, rt)
    if self.registers[rs] == self.registers[rt] then
        error("Trap TEQ at PC: "..tostring(self.PC))
    end
end

-- TEQI rs, imm
EXCEPT[0x0E] = function(self, rs, _, _, _, imm)
    if self.registers[rs] == imm then
        error("Trap TEQI at PC: "..tostring(self.PC))
    end
end

-- TNE rs, rt
EXCEPT[0x36] = function(self, rs, rt)
    if self.registers[rs] ~= self.registers[rt] then
        error("Trap TNE at PC: "..tostring(self.PC))
    end
end

-- TNEI rs, imm
EXCEPT[0x0F] = function(self, rs, _, _, _, imm)
    if self.registers[rs] ~= imm then
        error("Trap TNEI at PC: "..tostring(self.PC))
    end
end

-- TLT rs, rt
EXCEPT[0x30] = function(self, rs, rt)
    if self.registers[rs] < self.registers[rt] then
        error("Trap TLT at PC: "..tostring(self.PC))
    end
end

-- TLTI rs, imm
EXCEPT[0x08] = function(self, rs, _, _, _, imm)
    if self.registers[rs] < imm then
        error("Trap TLTI at PC: "..tostring(self.PC))
    end
end

-- TLTIU rs, imm
EXCEPT[0x09] = function(self, rs, _, _, _, imm)
    local a = self.registers[rs] & 0xFFFFFFFF
    local b = imm & 0xFFFFFFFF
    if a < b then
        error("Trap TLTIU at PC: "..tostring(self.PC))
    end
end

-- TLTU rs, rt
EXCEPT[0x32] = function(self, rs, rt)
    local a = self.registers[rs] & 0xFFFFFFFF
    local b = self.registers[rt] & 0xFFFFFFFF
    if a < b then
        error("Trap TLTU at PC: "..tostring(self.PC))
    end
end

-- TGE rs, rt
EXCEPT[0x31] = function(self, rs, rt)
    if self.registers[rs] >= self.registers[rt] then
        error("Trap TGE at PC: "..tostring(self.PC))
    end
end

-- TGEI rs, imm
EXCEPT[0x0A] = function(self, rs, _, _, _, imm)
    if self.registers[rs] >= imm then
        error("Trap TGEI at PC: "..tostring(self.PC))
    end
end

-- TGEIU rs, imm
EXCEPT[0x0B] = function(self, rs, _, _, _, imm)
    local a = self.registers[rs] & 0xFFFFFFFF
    local b = imm & 0xFFFFFFFF
    if a >= b then
        error("Trap TGEIU at PC: "..tostring(self.PC))
    end
end

-- TGEU rs, rt
EXCEPT[0x33] = function(self, rs, rt)
    local a = self.registers[rs] & 0xFFFFFFFF
    local b = self.registers[rt] & 0xFFFFFFFF
    if a >= b then
        error("Trap TGEU at PC: "..tostring(self.PC))
    end
end

-- Attach to CPU
R4300i.exceptions = EXCEPT

-- =============================
-- COP0
-- =============================
local COP0 = {}

-- MFC0 rt, fs  (Move from COP0)
COP0[0x00] = function(self, fs, rt)
    self.registers[rt] = self.CP0[fs] or 0
end

-- MTC0 rt, fs  (Move to COP0)
COP0[0x04] = function(self, fs, rt)
    self.CP0[fs] = self.registers[rt]
end

-- TLBP  (Probe TLB for matching entry)
COP0[0x08] = function(self)
    -- For simplicity, just simulate a TLB probe
    -- Normally sets index register on match/failure
    self.CP0.Index = 0
end

-- TLBR  (Read indexed TLB entry)
COP0[0x10] = function(self)
    -- Simulate reading TLB entry into CP0 registers
    -- Placeholder: no actual TLB implemented
end

-- TLBWI (Write indexed TLB entry)
COP0[0x18] = function(self)
    -- Simulate writing CP0 registers to TLB at index
end

-- TLBWR (Write random TLB entry)
COP0[0x1A] = function(self)
    -- Simulate writing CP0 registers to random TLB
end

-- ERET (Return from exception)
COP0[0x18] = function(self)
    self.PC = self.CP0.EPC or self.PC
end

-- Attach to CPU
R4300i.cop0 = COP0

-- =============================
-- FPU aka COP1
-- =============================
local FPU = {}

-- Helper: convert to float
local function toFloat(n) return n * 1.0 end

-- ABS.fmt fd, fs
FPU["ABS"] = function(self, fd, fs)
    self.FPR[fd] = math.abs(self.FPR[fs])
end

-- MOV.fmt fd, fs
FPU["MOV"] = function(self, fd, fs)
    self.FPR[fd] = self.FPR[fs]
end

-- NEG.fmt fd, fs
FPU["NEG"] = function(self, fd, fs)
    self.FPR[fd] = -self.FPR[fs]
end

-- ADD.fmt fd, fs, ft
FPU["ADD"] = function(self, fd, fs, ft)
    self.FPR[fd] = self.FPR[fs] + self.FPR[ft]
end

-- SUB.fmt fd, fs, ft
FPU["SUB"] = function(self, fd, fs, ft)
    self.FPR[fd] = self.FPR[fs] - self.FPR[ft]
end

-- MUL.fmt fd, fs, ft
FPU["MUL"] = function(self, fd, fs, ft)
    self.FPR[fd] = self.FPR[fs] * self.FPR[ft]
end

-- DIV.fmt fd, fs, ft
FPU["DIV"] = function(self, fd, fs, ft)
    self.FPR[fd] = self.FPR[fs] / self.FPR[ft]
end

-- SQRT.fmt fd, fs
FPU["SQRT"] = function(self, fd, fs)
    self.FPR[fd] = math.sqrt(self.FPR[fs])
end

-- CEIL.L.fmt / CEIL.W.fmt
FPU["CEIL_L"] = function(self, fd, fs)
    self.FPR[fd] = math.ceil(self.FPR[fs])
end
FPU["CEIL_W"] = FPU["CEIL_L"]

-- FLOOR.L.fmt / FLOOR.W.fmt
FPU["FLOOR_L"] = function(self, fd, fs)
    self.FPR[fd] = math.floor(self.FPR[fs])
end
FPU["FLOOR_W"] = FPU["FLOOR_L"]

-- ROUND.L.fmt / ROUND.W.fmt
FPU["ROUND_L"] = function(self, fd, fs)
    self.FPR[fd] = math.floor(self.FPR[fs] + 0.5)
end
FPU["ROUND_W"] = FPU["ROUND_L"]

-- TRUNC.L.fmt / TRUNC.W.fmt
FPU["TRUNC_L"] = function(self, fd, fs)
    self.FPR[fd] = math.floor(self.FPR[fs])
end
FPU["TRUNC_W"] = FPU["TRUNC_L"]

-- CVT.S.fmt fd, fs
FPU["CVT_S"] = function(self, fd, fs)
    self.FPR[fd] = toFloat(self.FPR[fs])
end

-- CVT.D.fmt fd, fs
FPU["CVT_D"] = function(self, fd, fs)
    self.FPR[fd] = toFloat(self.FPR[fs])
end

-- CVT.W.fmt fd, fs
FPU["CVT_W"] = function(self, fd, fs)
    self.FPR[fd] = math.floor(self.FPR[fs])
end

-- CVT.L.fmt fd, fs
FPU["CVT_L"] = function(self, fd, fs)
    self.FPR[fd] = math.floor(self.FPR[fs])
end

-- LWC1 ft, offset(base)
FPU["LWC1"] = function(self, ft, base, offset)
    local addr = self.registers[base] + offset
    self.FPR[ft] = self.memory[addr] or 0
end

-- SWC1 ft, offset(base)
FPU["SWC1"] = function(self, ft, base, offset)
    local addr = self.registers[base] + offset
    self.memory[addr] = self.FPR[ft]
end

-- DMFC1 rt, fs
FPU["DMFC1"] = function(self, rt, fs)
    self.registers[rt] = self.FPR[fs]
end

-- DMTC1 rt, fs
FPU["DMTC1"] = function(self, rt, fs)
    self.FPR[fs] = self.registers[rt]
end

-- C.cond.fmt fs, ft  (floating point compare)
FPU["C_EQ"] = function(self, fs, ft)
    self.FCR[0] = (self.FPR[fs] == self.FPR[ft]) and 1 or 0
end
FPU["C_LT"] = function(self, fs, ft)
    self.FCR[0] = (self.FPR[fs] < self.FPR[ft]) and 1 or 0
end
FPU["C_LE"] = function(self, fs, ft)
    self.FCR[0] = (self.FPR[fs] <= self.FPR[ft]) and 1 or 0
end

-- BC1T offset  (branch on FP true)
FPU["BC1T"] = function(self, offset)
    if self.FCR[0] ~= 0 then
        self.PC = self.PC + (offset << 2)
    end
end

-- BC1F offset  (branch on FP false)
FPU["BC1F"] = function(self, offset)
    if self.FCR[0] == 0 then
        self.PC = self.PC + (offset << 2)
    end
end

-- BC1TL / BC1FL (likely)
FPU["BC1TL"] = FPU["BC1T"]
FPU["BC1FL"] = FPU["BC1F"]

-- CFC1 rt, fs
FPU["CFC1"] = function(self, rt, fs)
    self.registers[rt] = self.FCR[fs] or 0
end

-- CTC1 rt, fs
FPU["CTC1"] = function(self, rt, fs)
    self.FCR[fs] = self.registers[rt]
end

-- Attach FPU to CPU
R4300i.cop1 = FPU

-- ============================
-- The Grand Finale! PSUEDO!
-- ============================
local PSEUDO = {}

-- NOP -> SLL r0, r0, 0
PSEUDO["NOP"] = function(self)
    self.registers[0] = 0
end

-- MOVE rd, rs -> ADD rd, r0, rs
PSEUDO["MOVE"] = function(self, rd, rs)
    self.registers[rd] = self.registers[rs]
end

-- NEG rd, rt -> SUB rd, r0, rt
PSEUDO["NEG"] = function(self, rd, rt)
    self.registers[rd] = 0 - self.registers[rt]
end

-- NEGU rd, rs -> SUBU rd, r0, rs
PSEUDO["NEGU"] = function(self, rd, rs)
    self.registers[rd] = 0 - self.registers[rs]
end

-- BNEZ rs, offset -> BNE rs, r0, offset
PSEUDO["BNEZ"] = function(self, rs, offset)
    if self.registers[rs] ~= 0 then
        self.PC = self.PC + (offset << 2)
    end
end

-- BNEZL rs, offset -> BNEL rs, r0, offset
PSEUDO["BNEZL"] = function(self, rs, offset)
    if self.registers[rs] ~= 0 then
        self.PC = self.PC + (offset << 2)
    else
        self.PC = self.PC + 4
    end
end

-- BEQZ rs, offset -> BEQ rs, r0, offset
PSEUDO["BEQZ"] = function(self, rs, offset)
    if self.registers[rs] == 0 then
        self.PC = self.PC + (offset << 2)
    end
end

-- BEQZL rs, offset -> BEQL rs, r0, offset
PSEUDO["BEQZL"] = function(self, rs, offset)
    if self.registers[rs] == 0 then
        self.PC = self.PC + (offset << 2)
    else
        self.PC = self.PC + 4
    end
end

-- B offset -> BEQ r0, r0, offset
PSEUDO["B"] = function(self, offset)
    self.PC = self.PC + (offset << 2)
end

-- BAL offset -> BGEZAL r0, offset
PSEUDO["BAL"] = function(self, offset)
    self.registers[31] = self.PC + 8
    self.PC = self.PC + (offset << 2)
end

-- LI rt, imm -> ORI/ADDIU/LUI depending on imm size
PSEUDO["LI"] = function(self, rt, imm)
    if imm <= 0xFFFF then
        self.registers[rt] = imm
    else
        local high = (imm >> 16) & 0xFFFF
        local low = imm & 0xFFFF
        self.registers[rt] = (high << 16) | low
    end
end

-- S.S ft, offset(base) -> SWC1 ft, offset(base)
PSEUDO["S_S"] = function(self, ft, base, offset)
    self.cop1["SWC1"](self, ft, base, offset)
end

-- L.S ft, offset(base) -> LWC1 ft, offset(base)
PSEUDO["L_S"] = function(self, ft, base, offset)
    self.cop1["LWC1"](self, ft, base, offset)
end

-- Attach to CPU
R4300i.pseudo = PSEUDO

-- ============================
-- Execute Opcodes (FINAL)
-- ============================
function R4300i:execute(opcode, rs, rt, rd, sa, imm_or_target)
    -- Try each handler table in order
    local handler = self.loadstore[opcode]
                or self.arithmetic[opcode]
                or self.jumpbranch[opcode]
                or self.special[opcode]
                or self.exceptions[opcode]
                or self.cop0[opcode]
                or self.pseudo[opcode]
                -- For FPU, we need to pass instruction name instead of numeric opcode
                -- Assume imm_or_target can be string for FPU instructions
                or (type(imm_or_target) == "string" and self.cop1[imm_or_target])

    if handler then
        handler(self, rs, rt, rd, sa, imm_or_target)
    else
        error(string.format("Opcode 0x%X not implemented!", opcode))
    end
end
