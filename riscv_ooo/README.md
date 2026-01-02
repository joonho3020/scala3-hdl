# RISCV Core

## Frontend: Typical 4-stage Pipeline

- S0: icache tag lookup
- S1: addr translation & icache tag matching & data array lookup on hit
- S2: hit data provided to frontend
- S3: omitted for now, should predecode and perform frontend redirects for jumps & branches
- FB: fetch buffer

```text
S0 S1 S2 FB
0  x  x
4  0  x
8  4  0 <-miss
0  x  x
```

## Backend:

```text
+----------------------+--------------------------------------+
| Pipeline Stage       | HW Structures / Queues                |
+----------------------+--------------------------------------+
| Fetch                | Fetch Buffer (FB), I$                 |
| Decode + Rename 1    | Rename Table, Busy Table, FreeList    |
| Rename 2 + Dispatch  | ROB, Issue Queue, LDQ/STQ             |
| Issue                | Wakeup + Select                       |
| PRF Read             | Physical Register File                |
| Execute              | Functional Units                      |
| Mem                  | DCache                                |
| Writeback            | PRF writeback                         |
| Commit               | ROB commit/retire                     |
+----------------------+--------------------------------------+
```
