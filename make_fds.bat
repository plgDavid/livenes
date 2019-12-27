del live.fds
asm6 file.asm -dFDS -dCHRRAM
IF EXIST file.bin. (
copy /b fds.hed+file.bin live.fds
Nintendulator live.fds
)