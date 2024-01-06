# AUC Open H/W Lab (AUCOHL) IP Library
A library of commonly used re-usable components (in Verilog HDL) used to construct larger IPs.

## aucohl_sync
A brute force synchronizer with a a parameterized number of stages.

## aucohl_ped
A positive edge detector that output port (``out``) a pulse (for one clock cycle) when a positive edge is detected on the input port (``in``).

## aucohl_ned
A negative edge detector that output port (``out``) a pulse (for one clock cycle) when a negative edge is detected on the input port (``in``).

## aucohl_ticker
A tick generator a periodic ``tick`` output which can be used to enable periodic operations at a rate slower than the clock frequency. For an example, a counter that is incremented every ``4`` clock cycles can use the ticker to generate a count enable signal every ``4`` clock cycle. The input port ``clk_div`` minus ``1`` provides the clock frequency divisor to set the ``tick`` rate. 

## aucohl_glitch_filter
A glitch filter is used to remove unwanted pulses from a digital input signal that is usually high or low. Electrical or in some cases even mechanical interference can trigger an unwanted glitch pulse from the receiver. 
The glitch filter outputs a ‘1’ only when the current and previous ``N`` samples (``N`` is a module parameter) are ``1``, and a ``0`` only when the current and previous ``N`` samples are ``0``. Otherwise the output is unchanged from its current value.
The signal sampling rate is controlled by the ``CLKDIV`` parameter. The sampling rate is: $(clk frequency)/(CLKDIV-1)$.

## aucohl_fifo 
A parameterized FIFO. The parameter ``DW`` sets the word size (default: ``8``) and the parameter ``AW`` is used to provide the depth (how many words in the FIFO); the FIFO depth is $2^{AW}$.