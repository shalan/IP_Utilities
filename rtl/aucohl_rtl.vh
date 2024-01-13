`timescale          1ns/1ps
`default_nettype    none

`define SYNC_BLOCK(clk, rst_n, var, init)       always @(posedge clk, negedge rst_n)\
                                                    if(!rst_n) var <= init;\
                                                    else
                                                    