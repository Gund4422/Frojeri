-- ==========================
-- Vorx64: 64-bit integer library (embedded)
-- ==========================
local Vorx64 = {}
Vorx64.__index = Vorx64

function Vorx64.new(hi, lo)
    return setmetatable({hi = hi or 0, lo = lo or 0}, Vorx64)
end

function Vorx64:add(other)
    local lo = (self.lo + other.lo) & 0xFFFFFFFF
    local carry = ((self.lo + other.lo) >> 32) & 0xFFFFFFFF
    local hi = (self.hi + other.hi + carry) & 0xFFFFFFFF
    return Vorx64.new(hi, lo)
end

function Vorx64:sub(other)
    local lo = self.lo - other.lo
    local borrow = 0
    if lo < 0 then lo = lo + 0x100000000; borrow = 1 end
    local hi = (self.hi - other.hi - borrow) & 0xFFFFFFFF
    return Vorx64.new(hi, lo)
end

function Vorx64:band(other) return Vorx64.new(self.hi & other.hi, self.lo & other.lo) end
function Vorx64:bor(other) return Vorx64.new(self.hi | other.hi, self.lo | other.lo) end
function Vorx64:bxor(other) return Vorx64.new(self.hi ~ other.hi, self.lo ~ other.lo) end

function Vorx64:lshift(bits)
    bits = bits % 64
    if bits == 0 then return Vorx64.new(self.hi, self.lo) end
    if bits < 32 then
        local hi = ((self.hi << bits) | (self.lo >> (32 - bits))) & 0xFFFFFFFF
        local lo = (self.lo << bits) & 0xFFFFFFFF
        return Vorx64.new(hi, lo)
    else
        local hi = (self.lo << (bits - 32)) & 0xFFFFFFFF
        local lo = 0
        return Vorx64.new(hi, lo)
    end
end

function Vorx64:rshift(bits)
    bits = bits % 64
    if bits == 0 then return Vorx64.new(self.hi, self.lo) end
    if bits < 32 then
        local lo = ((self.lo >> bits) | (self.hi << (32 - bits))) & 0xFFFFFFFF
        local hi = (self.hi >> bits) & 0xFFFFFFFF
        return Vorx64.new(hi, lo)
    else
        local lo = (self.hi >> (bits - 32)) & 0xFFFFFFFF
        local hi = 0
        return Vorx64.new(hi, lo)
    end
end

function Vorx64:sar(bits)
    bits = bits % 64
    local sign = (self.hi & 0x80000000) ~= 0 and 0xFFFFFFFF or 0
    if bits == 0 then return Vorx64.new(self.hi, self.lo) end
    if bits < 32 then
        local lo = ((self.lo >> bits) | (self.hi << (32 - bits))) & 0xFFFFFFFF
        local hi = ((self.hi >> bits) | (sign << (32 - bits))) & 0xFFFFFFFF
        return Vorx64.new(hi, lo)
    else
        local hi = sign
        local lo = ((self.hi >> (bits - 32)) | (sign << (64 - bits))) & 0xFFFFFFFF
        return Vorx64.new(hi, lo)
    end
end

function Vorx64:cmp(other)
    if self.hi > other.hi then return 1
    elseif self.hi < other.hi then return -1
    elseif self.lo > other.lo then return 1
    elseif self.lo < other.lo then return -1
    else return 0
    end
end

function Vorx64:tonumber() return self.hi * 0x100000000 + self.lo end
function Vorx64:tohex() return string.format("%08X%08X", self.hi, self.lo) end

-- ==========================
-- frojeri
-- ==========================
local R4300i = {}
R4300i.__index = R4300i

function R4300i.new()
    local self = setmetatable({}, R4300i)

    -- 64-bit General Purpose Registers
    self.registers = {}
    for i = 0, 31 do
        self.registers[i] = Vorx64.new(0,0)
    end

    -- HI/LO 64-bit
    self.HI = Vorx64.new(0,0)
    self.LO = Vorx64.new(0,0)

    -- Program Counter
    self.PC = 0xBFC00000

    -- Branch delay support
    self.branchTarget = nil
    self.inDelaySlot = false

    -- COP0 minimal state
    self.cop0 = {EPC=0, Status=0x18000000, Cause=0}

    -- Memory and opcode tables
    self.memory = {}
    self.loadstore = {}
    self.arithmetic = {}
    self.jumpbranch = {}
    self.special = {}
    self.exceptions = {}
    self.cop1 = {}
    self.pseudo = {}
    self.linkedAddress = nil

    return self
end

return R4300i

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
-- Vorx64-aware Arithmetic Module (AM)
-- =============================
local AM = {}

-- ===== Basic Arithmetic =====

-- ADD rd, rs, rt (signed, with overflow ignored for now)
AM[0x20] = function(self, rd, rs, rt)
    self.registers[rd] = self.registers[rs]:add(self.registers[rt])
end

-- ADDU rd, rs, rt (unsigned)
AM[0x21] = function(self, rd, rs, rt)
    self.registers[rd] = self.registers[rs]:add(self.registers[rt])
end

-- SUB rd, rs, rt
AM[0x22] = function(self, rd, rs, rt)
    self.registers[rd] = self.registers[rs]:sub(self.registers[rt])
end

-- SUBU rd, rs, rt
AM[0x23] = function(self, rd, rs, rt)
    self.registers[rd] = self.registers[rs]:sub(self.registers[rt])
end

-- ===== Immediate Arithmetic =====

-- ADDI rt, rs, imm
AM[0x08] = function(self, rt, rs, _, _, imm)
    self.registers[rt] = self.registers[rs]:add(Vorx64.new(0, imm))
end

-- ADDIU rt, rs, imm
AM[0x09] = function(self, rt, rs, _, _, imm)
    self.registers[rt] = self.registers[rs]:add(Vorx64.new(0, imm))
end

-- SLTI rt, rs, imm
AM[0x0A] = function(self, rt, rs, _, _, imm)
    self.registers[rt] = self.registers[rs]:cmp(Vorx64.new(0, imm)) < 0 and 1 or 0
end

-- SLTIU rt, rs, imm
AM[0x0B] = function(self, rt, rs, _, _, imm)
    self.registers[rt] = self.registers[rs]:cmp(Vorx64.new(0, imm)) < 0 and 1 or 0
end

-- ANDI rt, rs, imm
AM[0x0C] = function(self, rt, rs, _, _, imm)
    self.registers[rt] = self.registers[rs]:band(Vorx64.new(0, imm))
end

-- ORI rt, rs, imm
AM[0x0D] = function(self, rt, rs, _, _, imm)
    self.registers[rt] = self.registers[rs]:bor(Vorx64.new(0, imm))
end

-- XORI rt, rs, imm
AM[0x0E] = function(self, rt, rs, _, _, imm)
    self.registers[rt] = self.registers[rs]:bxor(Vorx64.new(0, imm))
end

-- LUI rt, imm
AM[0x0F] = function(self, rt, _, _, _, imm)
    self.registers[rt] = Vorx64.new(0, imm << 16)
end

-- ===== Logic and Comparison =====

-- AND rd, rs, rt
AM[0x24] = function(self, rd, rs, rt)
    self.registers[rd] = self.registers[rs]:band(self.registers[rt])
end

-- OR rd, rs, rt
AM[0x25] = function(self, rd, rs, rt)
    self.registers[rd] = self.registers[rs]:bor(self.registers[rt])
end

-- XOR rd, rs, rt
AM[0x26] = function(self, rd, rs, rt)
    self.registers[rd] = self.registers[rs]:bxor(self.registers[rt])
end

-- NOR rd, rs, rt
AM[0x27] = function(self, rd, rs, rt)
    local ones = Vorx64.new(0xFFFFFFFF, 0xFFFFFFFF)
    self.registers[rd] = self.registers[rs]:bor(self.registers[rt]):bxor(ones)
end

-- SLT rd, rs, rt
AM[0x2A] = function(self, rd, rs, rt)
    self.registers[rd] = self.registers[rs]:cmp(self.registers[rt]) < 0 and 1 or 0
end

-- SLTU rd, rs, rt
AM[0x2B] = function(self, rd, rs, rt)
    self.registers[rd] = self.registers[rs]:cmp(self.registers[rt]) < 0 and 1 or 0
end

-- ===== Shift Instructions =====

-- SLL rd, rt, sa
AM[0x00] = function(self, rd, _, rt, sa)
    self.registers[rd] = self.registers[rt]:lshift(sa)
end

-- SRL rd, rt, sa
AM[0x02] = function(self, rd, _, rt, sa)
    self.registers[rd] = self.registers[rt]:rshift(sa)
end

-- SRA rd, rt, sa
AM[0x03] = function(self, rd, _, rt, sa)
    self.registers[rd] = self.registers[rt]:sar(sa)
end

-- SLLV rd, rt, rs
AM[0x04] = function(self, rd, rs, rt)
    local sa = self.registers[rs]:tonumber() & 0x1F
    self.registers[rd] = self.registers[rt]:lshift(sa)
end

-- SRLV rd, rt, rs
AM[0x06] = function(self, rd, rs, rt)
    local sa = self.registers[rs]:tonumber() & 0x1F
    self.registers[rd] = self.registers[rt]:rshift(sa)
end

-- SRAV rd, rt, rs
AM[0x07] = function(self, rd, rs, rt)
    local sa = self.registers[rs]:tonumber() & 0x1F
    self.registers[rd] = self.registers[rt]:sar(sa)
end

-- ===== Multiply / Divide =====

-- MULT rs, rt
AM[0x18] = function(self, rs, rt)
    local result = self.registers[rs]:mul(self.registers[rt])
    self.HI = result
    self.LO = result
end

-- MULTU rs, rt
AM[0x19] = function(self, rs, rt)
    local a, b = self.registers[rs], self.registers[rt]
    self.HI = a:mul(b)
    self.LO = a:mul(b)
end

-- DIV rs, rt
AM[0x1A] = function(self, rs, rt)
    local a, b = self.registers[rs], self.registers[rt]
    self.LO = Vorx64.new(0, math.floor(a:tonumber() / b:tonumber()))
    self.HI = Vorx64.new(0, a:tonumber() % b:tonumber())
end

-- DIVU rs, rt
AM[0x1B] = function(self, rs, rt)
    local a, b = self.registers[rs], self.registers[rt]
    self.LO = Vorx64.new(0, math.floor(a:tonumber() / b:tonumber()))
    self.HI = Vorx64.new(0, a:tonumber() % b:tonumber())
end

-- DMULT rs, rt
AM[0x1C] = function(self, rs, rt)
    local result = self.registers[rs]:mul(self.registers[rt])
    self.HI = result
    self.LO = result
end

-- DMULTU rs, rt
AM[0x1D] = function(self, rs, rt)
    local a, b = self.registers[rs], self.registers[rt]
    local result = a:mul(b)
    self.HI = result
    self.LO = result
end

-- DDIV rs, rt
AM[0x1E] = function(self, rs, rt)
    local a, b = self.registers[rs], self.registers[rt]
    self.LO = Vorx64.new(0, math.floor(a:tonumber() / b:tonumber()))
    self.HI = Vorx64.new(0, a:tonumber() % b:tonumber())
end

-- DDIVU rs, rt
AM[0x1F] = function(self, rs, rt)
    local a, b = self.registers[rs], self.registers[rt]
    self.LO = Vorx64.new(0, math.floor(a:tonumber() / b:tonumber()))
    self.HI = Vorx64.new(0, a:tonumber() % b:tonumber())
end

-- ===== Move HI/LO =====

-- MFHI rd
AM[0x10] = function(self, rd)
    self.registers[rd] = self.HI
end

-- MFLO rd
AM[0x12] = function(self, rd)
    self.registers[rd] = self.LO
end

-- MTHI rs
AM[0x11] = function(self, rs)
    self.HI = self.registers[rs]
end

-- MTLO rs
AM[0x13] = function(self, rs)
    self.LO = self.registers[rs]
end

-- Attach AM
R4300i.arithmetic = AM

-- =============================
-- Jump/Branch Module (JB) with delay-slot
-- =============================
local JB = {}

-- Helper: execute branch with delay slot
local function branch(self, target)
    self.branchTarget = target      -- set target for after delay slot
    self.inDelaySlot = true         -- flag delay slot
end

local zero = Vorx64.new(0, 0)

-- Standard branches
JB[0x04] = function(self, rs, rt, _, _, offset) -- BEQ
    if self.registers[rs]:cmp(self.registers[rt]) == 0 then
        branch(self, self.PC:add(Vorx64.new(0, offset << 2)))
    end
end

JB[0x05] = function(self, rs, rt, _, _, offset) -- BNE
    if self.registers[rs]:cmp(self.registers[rt]) ~= 0 then
        branch(self, self.PC:add(Vorx64.new(0, offset << 2)))
    end
end

JB[0x06] = function(self, rs, _, _, _, offset) -- BLEZ
    if self.registers[rs]:cmp(zero) <= 0 then
        branch(self, self.PC:add(Vorx64.new(0, offset << 2)))
    end
end

JB[0x07] = function(self, rs, _, _, _, offset) -- BGTZ
    if self.registers[rs]:cmp(zero) > 0 then
        branch(self, self.PC:add(Vorx64.new(0, offset << 2)))
    end
end

-- BLTZ / BGEZ / BLTZAL / BGEZAL
JB[0x01] = function(self, rs, _, _, _, offset, linkType)
    -- Determine variant by linkType (0 = BLTZ, 1 = BGEZ, 2 = BLTZAL, 3 = BGEZAL)
    linkType = linkType or 0
    local takeBranch = false
    if linkType == 0 or linkType == 2 then
        takeBranch = self.registers[rs]:cmp(zero) < 0
    else
        takeBranch = self.registers[rs]:cmp(zero) >= 0
    end
    if linkType == 2 or linkType == 3 then
        self.registers[31] = self.PC:add(Vorx64.new(0, 8))
    end
    if takeBranch then
        branch(self, self.PC:add(Vorx64.new(0, offset << 2)))
    end
end

-- Branch likely variants (execute next instruction only if branch taken)
local function branchLikely(self, rs, rt, offset, taken)
    if taken then
        branch(self, self.PC:add(Vorx64.new(0, offset << 2)))
    else
        -- skip delay slot
        self.PC = self.PC:add(Vorx64.new(0, 4))
    end
end

-- BLTZL / BGEZL
JB[0x10] = function(self, rs, _, _, _, offset, link)
    local taken = self.registers[rs]:cmp(zero) < 0
    if link then self.registers[31] = self.PC:add(Vorx64.new(0, 8)) end
    branchLikely(self, rs, nil, offset, taken)
end

JB[0x11] = function(self, rs, _, _, _, offset, link)
    local taken = self.registers[rs]:cmp(zero) >= 0
    if link then self.registers[31] = self.PC:add(Vorx64.new(0, 8)) end
    branchLikely(self, rs, nil, offset, taken)
end

-- J / JAL
JB[0x02] = function(self, _, _, _, _, target)
    local newPC = Vorx64.new(0, bit32.band(self.PC:tonumber(), 0xF0000000) + (target << 2))
    branch(self, newPC)
end

JB[0x03] = function(self, _, _, _, _, target)
    self.registers[31] = self.PC:add(Vorx64.new(0, 8))
    local newPC = Vorx64.new(0, bit32.band(self.PC:tonumber(), 0xF0000000) + (target << 2))
    branch(self, newPC)
end

-- JR / JALR
JB[0x08] = function(self, rs)
    branch(self, self.registers[rs])
end

JB[0x09] = function(self, rs, _, rd)
    rd = rd or 31
    self.registers[rd] = self.PC:add(Vorx64.new(0, 8))
    branch(self, self.registers[rs])
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
    -- Fetch handler from tables
    local handler = self.loadstore[opcode]
                or self.arithmetic[opcode]
                or self.jumpbranch[opcode]
                or self.special[opcode]
                or self.exceptions[opcode]
                or self.cop0[opcode]
                or self.pseudo[opcode]
                or (type(imm_or_target) == "string" and self.cop1[imm_or_target])

    if not handler then
        error(string.format("Opcode 0x%X not implemented!", opcode))
    end

    -- Save current PC (Vorx64) for normal increment
    local nextPC = self.PC:add(Vorx64.new(0, 4))

    -- Execute instruction
    handler(self, rs, rt, rd, sa, imm_or_target)

    -- Handle branch delay slot
    if self.inDelaySlot then
        -- Delay slot executed, commit branch target
        self.PC = self.branchTarget
        self.inDelaySlot = false
        self.branchTarget = nil
    else
        -- Normal PC increment
        self.PC = nextPC
    end
end
