# AUC Open H/W Lab (AUCOHL) IP Library
A library of commonly used re-usable components (in Verilog HDL) used to construct larger IPs.

## aucohl_sync
A brute force synchronizer with a a parameterized number of stages.
```Verilog
aucohl_sync #(parameter NUM_STAGES = 2) (
    input clk,
    input in,
    output out
);
```

## aucohl_ped
A positive edge detector that output port (``out``) a pulse (for one clock cycle) when a positive edge is detected on the input port (``in``).
```Verilog
module aucohl_ped (
    input clk,
    input in,
    output out
);
```

## aucohl_ned
A negative edge detector that output port (``out``) a pulse (for one clock cycle) when a negative edge is detected on the input port (``in``).
```Verilog
module aucohl_ned (
    input clk,
    input in,
    output out
);
```

## aucohl_ticker
A tick generator a periodic ``tick`` output which can be used to enable periodic operations at a rate slower than the clock frequency. For an example, a counter that is incremented every ``4`` clock cycles can use the ticker to generate a count enable signal every ``4`` clock cycle. 

The input port ``clk_div`` plus ``1`` provides the clock frequency divisor to set the ``tick`` rate. The ticker can be enabled and disabled using `en` port.

```Verilog
module aucohl_ticker #(parameter W=8) (
    input   wire            clk, 
    input   wire            rst_n,
    input   wire            en,
    input   wire [W-1:0]    clk_div,
    output  wire            tick
);
```

## aucohl_glitch_filter
Electrical or in some cases even mechanical interference can trigger an unwanted glitch pulses from the receiver. A glitch filter is used to remove unwanted pulses from a digital input signal that is usually high or low. 

The glitch filter outputs a `1` only when the current and previous ``N`` samples (``N`` is a module parameter) are ``1``, and a ``0`` only when the current and previous ``N`` samples are ``0``. Otherwise the output is unchanged from its current value.
The signal sampling rate is controlled by the ``CLKDIV`` parameter. The sampling rate is: $(clk frequency)/(CLKDIV+1)$.

```Verilog
module aucohl_glitch_filter #(parameter N = 8, CLKDIV = 1) (
    input   wire    clk,
    input   wire    rst_n,
    input   wire    in,
    output  reg     out
);
```
## aucohl_fifo 
A parameterized FIFO. The parameter ``DW`` sets the word size (default: ``8``) and the parameter ``AW`` is used to provide the depth (how many words in the FIFO); the FIFO depth is $2^{AW}$.

```Verilog
module aucohl_fifo #(parameter DW=8, AW=4)(
    input     wire            clk,
    input     wire            rst_n,
    input     wire            rd,
    input     wire            wr,
    input     wire [DW-1:0]   wdata,
    output    wire            empty,
    output    wire            full,
    output    wire [DW-1:0]   rdata,
    output    wire [AW-1:0]   level    
);
```
