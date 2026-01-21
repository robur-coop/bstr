### v0.0.4 (2025-01-20)

- Fix compilation of our C stubs (spotted by @hannesm, fixed by @dinosaure, [#10][10])

[10]: https://git.robur.coop/robur/bstr/pulls/10

### v0.0.3 (2025-11-20)

- Fix SIGSEGV when we use `memmove` (retry) (@dinosaure, [#4][4])
- Fix license and mention astring developpers (@dinosaure, [#5][5])
- Apply ocamlformat.0.28.1 (@dinosaure, [#7][7])
- Fix encoding of numbers with `Bin` (@swrup, @reynir, @dinosaure, [#8][8])
- Add `Bstr.cuts` (@dinosaure, [#6][6])

[4]: https://git.robur.coop/robur/bstr/pulls/4
[5]: https://git.robur.coop/robur/bstr/pulls/5
[7]: https://git.robur.coop/robur/bstr/pulls/7
[8]: https://git.robur.coop/robur/bstr/pulls/8
[6]: https://git.robur.coop/robur/bstr/pulls/6

### v0.0.2 (2025-06-23)

- Fix SIGSEGV when we use `memmove`

### v0.0.1 (2025-04-28)

- First release of `bstr`, `slice` & `bin`
