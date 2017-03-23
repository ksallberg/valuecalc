#valuecalc

Value based (haskell powered) investment calculations for the stock markets.

##Build:
stack build
##Run:
stack runghc Main
##Test:
stack test

How:
For each ticker, this program calculates the balance sheet value of the company
and compares it with the market cap to determine if the company is undervalued.
A positive difference value shows how much undervalued the company is.
A negative shows how overvalued it is.
