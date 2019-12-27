del live_UNROM.nes
asm6 file.asm -dUNROM -dCHRRAM
IF EXIST file.bin. (
move file.bin live_UNROM.nes
Nintendulator live_UNROM.nes
)
