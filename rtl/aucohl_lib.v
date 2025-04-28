/*
	Copyright 2020 AUCOHL

    Author: Mohamed Shalan (mshalan@aucegypt.edu)
	
	Licensed under the Apache License, Version 2.0 (the "License"); 
	you may not use this file except in compliance with the License. 
	You may obtain a copy of the License at:

	http://www.apache.org/licenses/LICENSE-2.0

	Unless required by applicable law or agreed to in writing, software 
	distributed under the License is distributed on an "AS IS" BASIS, 
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
	See the License for the specific language governing permissions and 
	limitations under the License.
*/

`timescale			    1ns/1ps
`default_nettype		none

`define     PED(clk, sig, pulse)    reg last_``sig``; always @(posedge clk) last_``sig`` <= sig; assign pulse = sig & ~last_``sig``;
`define     NED(clk, sig, pulse)    reg last_``sig``; always @(posedge clk) last_``sig`` <= sig; assign pulse = ~sig & last_``sig``;

/*
    Brute-force Synchronizer
*/
module aucohl_sync #(parameter NUM_STAGES = 2) (
    input wire clk,
    input wire in,
    output wire out
);

    reg [NUM_STAGES-1:0] sync;

    always @(posedge clk)
        sync <= {sync[NUM_STAGES-2:0], in};

    assign out = sync[NUM_STAGES-1];

endmodule

/*
    A positive edge detector
*/
module aucohl_ped (
    input wire clk,
    input wire in,
    output wire out
);
    reg last_in; 
    always @(posedge clk) 
	last_in <= in; 
    assign out = in & ~last_in;
endmodule

/*
    A negative edge detector
*/
module aucohl_ned (
    input wire clk,
    input wire in,
    output wire out
);
    reg last_in; 
    always @(posedge clk) 
        last_in <= in;
    assign out = ~in & last_in;
endmodule

/*
    A tick generator
*/
module aucohl_ticker #(parameter W=8) (
    input   wire            clk, 
    input   wire            rst_n,
    input   wire            en,
    input   wire [W-1:0]    clk_div,
    output  wire            tick
);

    reg [W-1:0] counter;
    wire        counter_is_zero = (counter == 'b0);
    wire        tick_w;
    reg         tick_reg;

    always @(posedge clk, negedge rst_n)
        if(~rst_n)
            counter <=  'b0;
        else if(en) 
            if(counter_is_zero)
                counter <=  clk_div;
            else
                counter <=  counter - 'b1; 

	assign tick_w = (clk_div == 'b0)  ?   1'b1 : counter_is_zero;

    always @(posedge clk or negedge rst_n)
        if(!rst_n)
            tick_reg <= 1'b0;
        else if(en)
            tick_reg <= tick_w;
        else
            tick_reg <= 0;

    assign tick = tick_reg;

endmodule

/*
    A glitch filter
*/
module aucohl_glitch_filter #(parameter N = 8, CLKDIV = 8'd1) (
    input   wire    clk,
    input   wire    rst_n,
    input   wire    in,
    input   wire    en,
    output  reg     out
);

    reg [N-1:0] shifter;
    wire        tick;

    aucohl_ticker ticker (
        .clk(clk),
        .rst_n(rst_n),
	.en(en),
        .clk_div(CLKDIV),
        .tick(tick)
    );

    always @(posedge clk, negedge rst_n)
        if(!rst_n)
            shifter <= 'b0;
        else if(tick)
            shifter <= {shifter[N-2:0], in};

    wire all_ones   = & shifter;
    wire all_zeros  = ~| shifter;

    always @(posedge clk, negedge rst_n)
        if(!rst_n)
            out <= 1'b0;
        else
            if(all_ones) 
                out <= 1'b1;
            else if(all_zeros) 
                out <= 1'b0;
endmodule

/*
    A FIFO
    Depth = 2^AW
    Width = DW
*/
module aucohl_fifo #(parameter DW=8, AW=4)(
    input   wire            clk,
    input   wire            rst_n,
    input   wire            rd,
    input   wire            wr,
    input   wire            flush,
    input   wire [DW-1:0]   wdata,
    output  wire            empty,
    output  wire            full,
    output  wire [DW-1:0]   rdata,
    output  wire [AW-1:0]   level    
);

    localparam  DEPTH = 2**AW;

    //Internal Signal declarations
    reg [DW-1:0]  array_reg [DEPTH-1:0];
    reg [AW-1:0]  w_ptr_reg;
    reg [AW-1:0]  w_ptr_next;
    reg [AW-1:0]  w_ptr_succ;
    reg [AW-1:0]  r_ptr_reg;
    reg [AW-1:0]  r_ptr_next;
    reg [AW-1:0]  r_ptr_succ;

    // Level
    reg [AW-1:0] level_reg;
    reg [AW-1:0] level_next;      
    reg full_reg;
    reg empty_reg;
    reg full_next;
    reg empty_next;

    wire w_en;

    always @ (posedge clk)
        if(w_en) begin
            array_reg[w_ptr_reg] <= wdata;
        end

    assign rdata = array_reg[r_ptr_reg];   
    assign w_en = wr & ~full_reg;           

    //State Machine
    always @ (posedge clk, negedge rst_n) begin 
        if(!rst_n)
            begin
                w_ptr_reg <= 'b0;
                r_ptr_reg <= 'b0;
                full_reg  <= 1'b0;
                empty_reg <= 1'b1;
                level_reg <= 'd0;
            end
        else if(flush)
            begin
                w_ptr_reg <= 'b0;
                r_ptr_reg <= 'b0;
                full_reg  <= 1'b0;
                empty_reg <= 1'b1;
                level_reg <= 'd0;
            end
        else
            begin
                w_ptr_reg <= w_ptr_next;
                r_ptr_reg <= r_ptr_next;
                full_reg  <= full_next;
                empty_reg <= empty_next;
                level_reg <= level_next;
            end
    end

    //Next State Logic
    always @* begin
        w_ptr_succ  =   w_ptr_reg + 1;
        r_ptr_succ  =   r_ptr_reg + 1;
        w_ptr_next  =   w_ptr_reg;
        r_ptr_next  =   r_ptr_reg;
        full_next   =   full_reg;
        empty_next  =   empty_reg;
        level_next  =   level_reg;

        case({w_en,rd})
            //2'b00: nop
            2'b01: 
                if(~empty_reg) begin
                    r_ptr_next = r_ptr_succ;
                    full_next = 1'b0;
                    level_next = level_reg - 1;
                    if (r_ptr_succ == w_ptr_reg)
                        empty_next = 1'b1;
                end
            
            2'b10: 
                if(~full_reg) begin
                    w_ptr_next = w_ptr_succ;
                    empty_next = 1'b0;
                    level_next = level_reg + 1;
                    if (w_ptr_succ == r_ptr_reg)
                        full_next = 1'b1;
                end
            
            2'b11: begin
                w_ptr_next = w_ptr_succ;
                r_ptr_next = r_ptr_succ;
            end
        endcase
    end

    //Set Full and Empty
    assign full = full_reg;
    assign empty = empty_reg;
    assign level = level_reg;
  
endmodule

module aucohl_sar_ctrl #(parameter SIZE = 8) ( 
    input   wire                clk,        // The clock
    input   wire                rst_n,      // Active high reset
    input   wire                soc,        // Start of Conversion
    input   wire                cmp,        // Analog comparator output
    input   wire                en,         // Enable the controller
    input   wire [3:0]          swidth,     // Number of clock cycles for sampling
    output  wire                sample_n,   // Sample_n/Hold
    output  wire [SIZE-1:0]     data,       // The output sample
    output  wire                eoc,        // End of Conversion
    output  wire                dac_rst     // reset the DAC capacitive array (optional)
);
	
	reg [SIZE-1:0]  result;
	reg [SIZE-1:0]  shift;
    reg [3:0]       sample_ctr;
    wire            sample_ctr_match = (swidth == sample_ctr);
	
    // FSM to handle the SAR operation
    // Idle -> Cap array reset (1) -> Sample (swidth) -> Convert (SIZE) -> Done (1)
    reg [2:0]   state, nstate;
	localparam [2:0]    IDLE    = 3'd0, 
	                    SAMPLE  = 3'd1, 
	                    CONV    = 3'd2, 
	                    DONE    = 3'd3,
                        RST     = 3'd7;

	always @*
        case (state)
            IDLE    :   if(soc) nstate = RST;
                        else nstate = IDLE;
            RST     :   nstate = SAMPLE;
            SAMPLE  :   if(sample_ctr_match) nstate = CONV;
                        else nstate = SAMPLE;
            CONV    :   if(shift == 1'b1) nstate = DONE;
                        else nstate = CONV;
            DONE    :   nstate = IDLE;
            default:    nstate = IDLE;
        endcase
	  
	always @(posedge clk or negedge rst_n)
        if(!rst_n)
            state <= IDLE;
        else if(en)
            state <= nstate;

    // Sample Counter
    always @(posedge clk or negedge rst_n)
        if(!rst_n)
            sample_ctr <= 'b0;
        else if(en & (state==SAMPLE))
            if(sample_ctr_match)
                sample_ctr <= 'b0;
            else
                sample_ctr <= sample_ctr + 'b1;
    
    // Shift Register
    always @(posedge clk)
        if(en)
            if(state == IDLE) 
                shift <= 1'b1 << (SIZE-1);
            else if(state == CONV)
                shift<= shift >> 1; 

    // The SAR
    wire [SIZE-1:0] current = (cmp == 1'b0) ? ~shift : {SIZE{1'b1}} ;
    wire [SIZE-1:0] next = shift >> 1;
    always @(posedge clk)
        if(en)
            if(state == IDLE)
                result <= 'b0;
            else if(state == RST) 
                result <= 1'b1 << (SIZE-1);
            else if(state == CONV)
                result <= (result | next) & current; 
	   
	assign data = result;
    
    assign eoc = (state==DONE);

    assign sample_n = (state != SAMPLE);


	assign dac_rst = (state == RST);

endmodule

/*
    Glitch-free 2x1 Clock Multiplexor
    based on the following old EDN article: 
    https://www.edn.com/techniques-to-make-clock-switching-glitch-free/
*/

module aucohl_clkmux_2x1 (
    input   wire    rst_n,
    input   wire    clk0, 
    input   wire    clk1,
    input   wire    sel,
    output  wire    clko
);

    reg Q1a, Q1b, Q2a, Q2b;
    wire q1a_in, q2a_in;
    
    assign clko = (clk0 & Q1b) | (clk1 & Q2b);
    
    wire  Q2b_bar = ~Q2b;
    wire  Q1b_bar = ~Q1b;
    wire  sel_bar = ~sel;
    
    assign q1a_in = Q2b_bar & sel_bar;
    assign q2a_in = Q1b_bar & sel;
    
    always @(posedge clk0 or negedge rst_n) 
        if (~rst_n) 
            Q1a <= 1'b0; 
        else 
            Q1a <= q1a_in;

    always @(negedge clk0 or negedge rst_n) 
        if (~rst_n) 
            Q1b <= 1'b0; 
        else 
            Q1b <= Q1a;

    always @(posedge clk1 or negedge rst_n) 
        if (~rst_n) 
            Q2a <= 1'b0; 
        else 
            Q2a <= q2a_in;

    always @(negedge clk1 or negedge rst_n) 
        if (~rst_n) 
            Q2b <= 1'b0; 
        else 
            Q2b <= Q2a;

endmodule

/*
    Glitch-free 4x1 Clock Multiplexor
*/
module aucohl_clkmux_4x1 (
    input   wire        rst_n,
    input   wire        clk0, 
    input   wire        clk1, 
    input   wire        clk2, 
    input   wire        clk3,
    input   wire [1:0]  sel,
    output  wire        clko
);

    wire clko1, clko2;

    aucohl_clkmux_2x1 m1(  
                    .rst_n(rst_n),
                    .clk0(clk0), 
                    .clk1(clk1), 
                    .clko(clko1), 
                    .sel(sel[0])
                );
    aucohl_clkmux_2x1 m2(  
                    .rst_n(rst_n),
                    .clk0(clk2), 
                    .clk1(clk3), 
                    .clko(clko2), 
                    .sel(sel[0])
                );
    aucohl_clkmux_2x1 m3(  
                    .rst_n(rst_n),
                    .clk0(clko1), 
                    .clk1(clko2), 
                    .clko(clko), 
                    .sel(sel[1])
                );
    
endmodule

/*
Clocking Gating Cell
*/
module ef_gating_cell(
    `ifdef USE_POWER_PINS 
    input   wire    vpwr,
    input   wire    vgnd,
    `endif // USE_POWER_PINS
    input   wire    clk,
    input   wire    rst_n,
    input   wire    clk_en,
    output  wire    clk_o
);
    `ifdef SKY130
    (* keep *) sky130_fd_sc_hd__dlclkp_4 clk_gate(
    `ifdef USE_POWER_PINS 
        .VPWR(vpwr), 
        .VGND(vgnd), 
        .VNB(vpwr),
		.VPB(vgnd),
    `endif // USE_POWER_PINS
        .GCLK(clk_o), 
        .GATE(clk_en), 
        .CLK(clk)
        );
    `else // SKY130
    assign clk_o = clk & clk_en; 
    `endif // SKY130
endmodule
