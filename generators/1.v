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




/*
    Brute-force Synchronizer
*/
module aucohl_sync #(parameter NUM_STAGES = 2) (
    input clk,
    input in,
    output out
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
    input clk,
    input in,
    output out
);
    reg last_in; always @(posedge clk) last_in <= in; assign out = in & ~last_in;
endmodule

/*
    A negative edge detector
*/
module aucohl_ned (
    input clk,
    input in,
    output out
);
    reg last_in; always @(posedge clk) last_in <= in; assign out = ~in & last_in;
endmodule

/*
    A tick generator
*/
module aucohl_ticker #(parameter W=8) (
    input   wire            clk, 
    input   wire            rst_n,
    input   wire [W-1:0]    clk_div,
    output  wire            tick
);

    reg [W-1:0] counter;
    wire        counter_is_zero = (counter == 'b0);
    always @(posedge clk, negedge rst_n)
        if(rst_n)
            counter <=  'b0;
        else if(counter_is_zero)
            counter <=  clk_div - 'b1;
        else
            counter <=  counter - 'b1; 

    assign tick = (clk_div == 'b1)  ?   1'b1 : counter_is_zero;
    
endmodule

/*
    A glitch filter
*/
module aucohl_glitch_filter #(parameter N = 8, CLKDIV = 1) (
    input   wire    clk,
    input   wire    rst_n,
    input   wire    in,
    output  reg     out
);

    reg [N-1:0] shifter;
    wire        tick;

    aucohl_ticker ticker (
        .clk(clk),
        .rst_n(rst_n),
        .clk_div(CLKDIV),
        .tick(tick)
    );

    always @(posedge clk, negedge rst_n)
        if(!rst_n)
            shifter = 'b0;
        else if(tick)
            shifter <= {shifter[N-2:0], in};

    wire all_ones   = & shifter;
    wire all_zeros  = | shifter;

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
*/
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
            level_reg <= 4'd0;
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
        w_ptr_succ = w_ptr_reg + 1;
        r_ptr_succ = r_ptr_reg + 1;

        w_ptr_next = w_ptr_reg;
        r_ptr_next = r_ptr_reg;
        full_next = full_reg;
        empty_next = empty_reg;
        level_next = level_reg;

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

/*
	Copyright 2020 AUCOHL. All rights reserved

	Author: Mohamed Shalan (mshalan@aucegypt.edu)
	
	Permission is hereby granted, free of charge, to any person obtaining
	a copy of this software and associated documentation files (the
	"Software"), to deal in the Software without restriction, including
	without limitation the rights to use, copy, modify, merge, publish,
	distribute, sublicense, and/or sell copies of the Software, and to
	permit persons to whom the Software is furnished to do so, subject to
	the following conditions:

	The above copyright notice and this permission notice shall be
	included in all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
	NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
	LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
	OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
	WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/
/*
    A Universal Asynchronous Receiver/Transmitter (UART) 
    - Programmable frame format
        - Data: 5-9 bits
        - Parity: None, Odd, Even, or Sticky at 0/1
    - TX and RX FIFOs with programmable thresholds
    - 16-bit prescaler (PR) for programable baud rate generation
    - Baudrate = CLK/((PR+1)*NUM_SAMPLES)
    - RX synchronizer
    - RX Glich Filter
    - Interrupt Sources:
        + TX fifo not full
        + RX fifo not empty
        + RX fifo level exceeded the threshold
        + TX fifo level is below the threshold
        + Framing Error
        + Parity Error
        + Break is observed
        + Timeout: Nothing received for the time of 4 frames!
        + Overrun
        + Receiving a specific frame
*/

`timescale			    1ns/1ps
`default_nettype		none


module AUCOHL_UART #(parameter  MDW = 9,        // Max data size/width
                                FAW = 4,        // FIFO Address width; Depth=2^AW
                                SC = 8,         // Number of samples per bit/baud
                                GFLEN = 8       // Length (number of stages) of the glitch filter
) (
    input   wire            clk,
    input   wire            rst_n,
    
    input   wire [15:0]     prescaler,
    input   wire            en,
    input   wire            tx_en,
    input   wire            rx_en,
    input   wire            rd,
    input   wire            wr,
    input   wire [MDW-1:0]  wdata,
    input   wire [3:0]      data_size,          // 5 - 9
    input   wire            stop_bits_count,    // 0: 1, 1: 2
    input   wire [2:0]      parity_type,        // 000: None, 001: odd, 010: even, 100: Sticky 0, 101: Sticky 1
    input   wire [3:0]      txfifotr,
    input   wire [3:0]      rxfifotr,
    input   wire [MDW-1:0]  match_data,
    input   wire [5:0]      timeout_bits,
    input   wire            loopback_en,
    input   wire            glitch_filter_en,
            
    output  wire            tx_empty,
    output  wire            tx_full,
    output  wire [FAW-1:0]  tx_level,
    output  wire            tx_level_below,
    output  wire [MDW-1:0]  rdata,
    output  wire            rx_empty,
    output  wire            rx_full,
    output  wire [FAW-1:0]  rx_level,
    output  wire            rx_level_above,

    output  wire            break_flag,
    output  wire            match_flag,
    output  wire            frame_error_flag,
    output  wire            parity_error_flag,
    output  wire            overrun_flag,
    output  wire            timeout_flag,

    input   wire            rx,
    output  wire            tx
);

    wire        tx_done;
    wire        rx_done;

    wire        b_tick;

    wire [MDW-1:0]  tx_data;
    wire [MDW-1:0]  rx_data;
    
    parameter FIFO_DW = MDW;

    wire        rx_synched;
    wire        rx_filtered;
    wire        rx_in;

    aucohl_sync rx_sync (
        .clk(clk),
        .in(rx),
        .out(rx_synched)
    );

    aucohl_glitch_filter #(.N(GFLEN)) rx_glitch_filter (
        .clk(clk),
        .rst_n(rst_n),
        .in(rx_synched),
        .out(rx_filtered)
    );

    assign rx_in =  loopback_en         ? tx            : 
                    glitch_filter_en    ? rx_filtered   : 
                    rx_synched;

    BAUDGEN buad_gen (
        .clk(clk),
        .rst_n(rst_n),
        .prescale(prescaler),
        .en(en),
        .baudtick(b_tick)
    );
  
    aucohl_fifo #(.DW(FIFO_DW), .AW(FAW)) fifo_tx (
        .clk(clk),
        .rst_n(rst_n),
        .rd(tx_done),
        .wr(wr),
        .wdata(wdata),
        .empty(tx_empty),
        .full(tx_full),
        .rdata(tx_data),
        .level(tx_level)
    );

    UART_TX #(.MDW(MDW), .NUM_SAMPLES(SC)) uart_tx (
        .clk(clk),
        .resetn(rst_n),
        .tx_start(~tx_empty),
        .b_tick(b_tick & tx_en),
        .data_size(data_size),
        .parity_type(parity_type),
        .stop_bits_count(stop_bits_count),
        .d_in(tx_data),
        .tx_done(tx_done),
        .tx(tx)
    );

    aucohl_fifo #(.DW(FIFO_DW), .AW(FAW)) fifo_rx (
        .clk(clk),
        .rst_n(rst_n),
        .rd(rd),
        .wr(rx_done),
        .wdata(rx_data),
        .empty(rx_empty),
        .full(rx_full),
        .rdata(rdata),
        .level(rx_level)
    );

    UART_RX #(.MDW(MDW), .NUM_SAMPLES(SC)) uart_rx (
        .clk(clk),
        .resetn(rst_n),
        .b_tick(b_tick & rx_en),
        .data_size(data_size),
        .parity_type(parity_type),
        .stop_bits_count(stop_bits_count),
        .match_data(match_data),
        .rx(rx_in),
        .break_flag(break_flag),
        .match_flag(match_flag),
        .parity_error(parity_error_flag),
        .frame_error(frame_error_flag),
        .rx_done(rx_done),
        .dout(rx_data)
    );

    reg [5:0]   bits_count;
    reg [4:0]   samples_count;
    always @ (posedge clk, negedge rst_n) begin
        if(!rst_n) begin
            bits_count <= 0;
            samples_count <= 0;
        end
        else if(b_tick)
            if(rx_done) bits_count <= 0;
            else if(samples_count == (SC - 1)) begin
                samples_count <= 0;
                if(timeout_flag)
                    bits_count <= 0;
                else
                    bits_count <= bits_count + 1;
            end else
                samples_count <= samples_count + 1'b1;
    end

    assign tx_level_below = (tx_level < txfifotr);
    assign rx_level_above = (rx_level > rxfifotr);
    assign overrun_flag = rx_full & rx_done;
    assign timeout_flag = (bits_count == timeout_bits);

endmodule


module BAUDGEN
(
    input   wire        clk,
    input   wire        rst_n,
    input   wire [15:0] prescale, 
    input   wire        en,
    output  wire        baudtick
);

    reg [15:0]  count_reg;
    wire [15:0] count_next;

    //Counter
    always @ (posedge clk, negedge rst_n) begin
        if(!rst_n)
            count_reg <= 0;
        else if(en)
            count_reg <= count_next;
    end

    assign count_next = ((count_reg == prescale) ? 0 : count_reg + 1'b1);
    assign baudtick = ((count_reg == prescale) ? 1'b1 : 1'b0);

endmodule

/*
    UART Receiver
*/
module UART_RX #(parameter NUM_SAMPLES = 16, MDW = 8)(
    input   wire            clk,
    input   wire            resetn,
    input   wire            b_tick,             // Baud generator tick
    input   wire [3:0]      data_size,          // 5 - 9
    input   wire            stop_bits_count,    // 0: 1, 1: 2
    input   wire [2:0]      parity_type,        // 000: None, 001: odd, 010: even, 
                                                // 100: Sticky 0, 101: Sticky 1
    input   wire            rx,                 // RS-232 data port
    input   wire [MDW-1:0]  match_data,
    output  reg             rx_done,            // Transfer completed
    output  wire            parity_error,       // Parity Error
    output  wire            frame_error,        // Framing Error
    output  wire            break_flag,         // Break flag
    output  wire            match_flag,
    output  wire [MDW-1:0]  dout                // Received data
);
    //STATE DEFINES  
    localparam [2:0] idle_st    = 3'b000;
    localparam [2:0] start_st   = 3'b001;
    localparam [2:0] data_st    = 3'b010;
    localparam [2:0] parity_st  = 3'b011;
    localparam [2:0] stop0_st   = 3'b100;
    localparam [2:0] stop1_st   = 3'b101;

    //Internal Signals  
    reg [2:0]   current_state;
    reg [2:0]   next_state;
    reg [3:0]   b_reg;            //baud-rate/over sampling counter
    reg [3:0]   b_next;
    reg [3:0]   count_reg;        //data-bit counter
    reg [2:0]   count_next;
    reg [8:0]   data_reg;         //data register
    reg [8:0]   data_next;
    reg         p_error_reg;
    reg         p_error_next;
    reg         f_error_reg;
    reg         f_error_next;

    //State Machine  
    always @ (posedge clk, negedge resetn) begin
        if(!resetn) begin
            current_state <= idle_st;
            b_reg <= 0;
            count_reg <= 0;
            data_reg <= 0;
            p_error_reg <= 0;
        end else begin
            current_state <= next_state;
            b_reg <= b_next;
            count_reg <= count_next;
            data_reg <= data_next;
            if(current_state == idle_st) 
                p_error_reg <= 0;
            else 
                if(p_error_next) 
                    p_error_reg <= p_error_next;
            if(current_state == idle_st) 
                    f_error_reg <= 0;
                else 
                    if(f_error_next) 
                        f_error_reg <= f_error_next;
        end
    end

    //Next State Logic 
    always @* begin
        next_state = current_state;
        b_next = b_reg;
        count_next = count_reg;
        data_next = data_reg;
        rx_done = 1'b0;
        p_error_next = 1'b0;
            
        case(current_state)
            idle_st:
                if(~rx & b_tick)
                begin
                    next_state = start_st;
                    b_next = 0;
                end
                
            start_st:
                if(b_tick)
                    if(b_reg == (NUM_SAMPLES/2 - 1)) begin
                        next_state = data_st;
                        b_next = 0;
                        count_next = 0;
                    end else
                        b_next = b_reg + 1'b1;
                    
            data_st:
                if(b_tick)
                    if(b_reg == (NUM_SAMPLES - 1)) begin
                        b_next = 0;
                        data_next = {rx, data_reg [(MDW-1):1]};
                        if(count_next == (data_size - 1)) 
                            if(parity_type == 3'b000)         
                                next_state = stop0_st;
                            else
                                next_state = parity_st;
                        else
                            count_next = count_reg + 1'b1;
                    end else
                        b_next = b_reg + 1;
            
            parity_st:
                if(b_tick)
                    if(b_reg == (NUM_SAMPLES - 1)) begin
                        b_next = 0;
                        next_state = stop0_st;
                        case (parity_type)
                            3'b001 : //Odd parity
                                if(~^dout != rx) p_error_next = 1;
                            3'b010 : //Even parity
                                if(^dout != rx) p_error_next = 1;
                            3'b100 : //Sticky 0 parity
                                if(1'b0 != rx) p_error_next = 1;
                            3'b101 : //Sticky 1 parity
                                if(1'b1 != rx) p_error_next = 1;
                        endcase
                    end else
                        b_next = b_reg + 1;  
            stop0_st:
                if(b_tick)
                    if(b_reg == (NUM_SAMPLES - 1)) begin 
                        b_next = 0;
                        if(!rx) f_error_next = 1;
                        if(stop_bits_count)         //Two stop bits
                            next_state = stop1_st;
                        else begin                  //One stop bit 
                            next_state = idle_st;
                            rx_done = 1'b1;
                        end
                    end else
                        b_next = b_reg + 1;
            stop1_st:
                if(b_tick)
                    if(b_reg == (NUM_SAMPLES - 1)) begin //Two stop bits
                        b_next = 0;
                        next_state = idle_st;
                        rx_done = 1'b1;
                        if(!rx) f_error_next = 1;
                    end else
                        b_next = b_reg + 1;
        endcase
    end
  
    // Break Detector
    reg [11:0] brk;
    always @ (posedge clk, negedge resetn) begin
        if(!resetn) 
            brk <= 12'hFFF;
        else if(b_tick)
            if(b_reg == (NUM_SAMPLES - 1)) begin
                if(current_state == idle_st)
                    brk <= 12'hFFF;
                else
                    brk <= {brk[10:0], rx};
            end
    end

    assign      dout            =   data_reg >> (9-data_size);
    assign      parity_error    =   p_error_reg & rx_done;
    assign      frame_error     =   f_error_reg & rx_done;
    assign      break_flag      =   (brk == 0);
    assign      match_flag      =   (match_data == dout) & rx_done;

endmodule

/*
    UART Transmitter
*/
module UART_TX #(parameter NUM_SAMPLES = 16, MDW = 8)(
    input   wire                clk,
    input   wire                resetn,
    input   wire                tx_start,        
    input   wire                b_tick,             //baud rate tick
    input   wire [3:0]          data_size,          // 5 - 9
    input   wire                stop_bits_count,    // 0: 1, 1: 2
    input   wire [2:0]          parity_type,        // 000: None, 001: odd, 010: even, 
                                                    // 100: Sticky 0, 101: Sticky 1
    input   wire [MDW-1:0]      d_in,               // input data to transmit
    output  reg                 tx_done,            // Transfer finished
    output  wire                tx                  // output data to RS-232
);
  
    //STATE DEFINES  
    localparam [2:0] idle_st    = 3'b000;
    localparam [2:0] start_st   = 3'b001;
    localparam [2:0] data_st    = 3'b010;
    localparam [2:0] parity_st  = 3'b011;
    localparam [2:0] stop0_st   = 3'b100;
    localparam [2:0] stop1_st   = 3'b101;
/*
    //STATE DEFINES  
    localparam [1:0] idle_st = 2'b00;
    localparam [1:0] start_st = 2'b01;
    localparam [1:0] data_st = 2'b11;
    localparam [1:0] stop_st = 2'b10;
*/
    //Internal Signals  
    reg [2:0]   current_state;
    reg [2:0]   next_state;
    reg [3:0]   b_reg;          // baud tick counter
    reg [3:0]   b_next;
    reg [2:0]   count_reg;      // data bit counter
    reg [2:0]   count_next;
    reg [8:0]   data_reg;       // data register
    reg [8:0]   data_next;
    reg         tx_reg;         // output data reg
    reg         tx_next;
  
    //State Machine  
    always @(posedge clk, negedge resetn) begin
        if(!resetn) begin
            current_state   <= idle_st;
            b_reg           <= 0;
            count_reg       <= 0;
            data_reg        <= 0;
            tx_reg          <= 1'b1;
        end else begin
            current_state   <= next_state;
            b_reg           <= b_next;
            count_reg       <= count_next;
            data_reg        <= data_next;
            tx_reg          <= tx_next;
        end
    end

    //Next State Logic  
    always @* begin
        next_state  =   current_state;
        tx_done     =   1'b0;
        b_next      =   b_reg;
        count_next  =   count_reg;
        data_next   =   data_reg;
        tx_next     =   tx_reg;
        
        case(current_state)
            idle_st: begin
                tx_next = 1'b1;
                if(tx_start) begin
                    next_state = start_st;
                    b_next = 0;
                    data_next = d_in;
                end
            end
            
            start_st: begin //send start bit
                tx_next = 1'b0;
                if(b_tick)
                    if(b_reg == (NUM_SAMPLES)) begin
                        next_state = data_st;
                        b_next = 0;
                        count_next = 0;
                    end
                    else
                        b_next = b_reg + 1;
            end
            
            data_st: begin //send data serially
                tx_next = data_reg[0];
                if(b_tick)
                    if(b_reg == (NUM_SAMPLES - 1)) begin
                        b_next = 0;
                        data_next = data_reg >> 1;
                        if(count_next == (data_size - 1)) 
                            if(parity_type == 3'b000)         
                                next_state = stop0_st;
                            else
                                next_state = parity_st;
                        else
                            count_next = count_reg + 1;
                    end
                    else
                        b_next = b_reg + 1;
            end
            
            parity_st: begin
                tx_next = 1'b0;
                if(b_tick)
                    if(b_reg == (NUM_SAMPLES - 1)) begin
                        b_next = 0;
                        next_state = stop0_st;
                        case (parity_type)
                            3'b001 : //Odd parity
                                tx_next = ~^d_in;
                            3'b010 : //Even parity
                                tx_next = ^d_in;
                            3'b100 : //Sticky 0 parity
                                tx_next = 0;
                            3'b101 : //Sticky 1 parity
                                tx_next = 1;
                        endcase
                    end else
                        b_next = b_reg + 1;
            end

            stop0_st: begin //send stop bit
                tx_next = 1'b1;
                if(b_tick)
                    if(b_reg == (NUM_SAMPLES - 1)) begin
                        b_next = 0;
                        if(stop_bits_count)         //Two stop bits
                                next_state = stop1_st;
                        else begin                  //One stop bit 
                            next_state = idle_st;
                            tx_done = 1'b1;
                        end        
                    end
                    else
                        b_next = b_reg + 1;
            end

            stop1_st: begin
                tx_next = 1'b1;
                if(b_tick)
                    if(b_reg == (NUM_SAMPLES - 1)) begin //Two stop bits
                        b_next = 0;
                        next_state = idle_st;
                        tx_done = 1'b1;
                    end else
                        b_next = b_reg + 1;
            end
        endcase
    end
  
    assign tx = tx_reg;
  
endmodule/*
	Copyright 2022 AUCOHL

	Author: Mohamed Shalan (mshalan@aucegypt.edu)

	Permission is hereby granted, free of charge, to any person obtaining
	a copy of this software and associated documentation files (the
	"Software"), to deal in the Software without restriction, including
	without limitation the rights to use, copy, modify, merge, publish,
	distribute, sublicense, and/or sell copies of the Software, and to
	permit persons to whom the Software is furnished to do so, subject to
	the following conditions:

	The above copyright notice and this permission notice shall be
	included in all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
	NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
	LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
	OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
	WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/

/* THIS FILE IS GENERATED, DO NOT EDIT */

`timescale			1ns/1ps
`default_nettype		none



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






                                        























module AUCOHL_UART_APB #( 
	parameter	
				SC = 8,
				MDW = 9,
				GFLEN = 8,
				FAW = 4
)(
	input wire          PCLK,
                                        input wire          PRESETn,
                                        input wire          PWRITE,
                                        input wire [31:0]   PWDATA,
                                        input wire [31:0]   PADDR,
                                        input wire          PENABLE,
                                        input wire          PSEL,
                                        output wire         PREADY,
                                        output wire [31:0]  PRDATA,
                                        output wire         IRQ
,
	input	[0:0]	rx,
	output	[0:0]	tx
);

	localparam	rxdata_REG_OFFSET = 16'd0;
	localparam	txdata_REG_OFFSET = 16'd4;
	localparam	prescaler_REG_OFFSET = 16'd12;
	localparam	control_REG_OFFSET = 16'd8;
	localparam	config_REG_OFFSET = 16'd16;
	localparam	fifo_control_REG_OFFSET = 16'd20;
	localparam	fifo_status_REG_OFFSET = 16'd24;
	localparam	match_REG_OFFSET = 16'd28;
	localparam	IM_REG_OFFSET = 16'd3840;
	localparam	MIS_REG_OFFSET = 16'd3844;
	localparam	RIS_REG_OFFSET = 16'd3848;
	localparam	IC_REG_OFFSET = 16'd3852;

	wire		clk = PCLK;
	wire		rst_n = PRESETn;


	wire		apb_valid   = PSEL & PENABLE;
                                        wire		apb_we	    = PWRITE & apb_valid;
                                        wire		apb_re	    = ~PWRITE & apb_valid;

	wire [16-1:0]	prescaler;
	wire [1-1:0]	en;
	wire [1-1:0]	tx_en;
	wire [1-1:0]	rx_en;
	wire [MDW-1:0]	wdata;
	wire [6-1:0]	timeout_bits;
	wire [1-1:0]	loopback_en;
	wire [1-1:0]	glitch_filter_en;
	wire [FAW-1:0]	tx_level;
	wire [FAW-1:0]	rx_level;
	wire [1-1:0]	rd;
	wire [1-1:0]	wr;
	wire [4-1:0]	data_size;
	wire [1-1:0]	stop_bits_count;
	wire [3-1:0]	parity_type;
	wire [FAW-1:0]	txfifotr;
	wire [FAW-1:0]	rxfifotr;
	wire [MDW-1:0]	match_data;
	wire [1-1:0]	tx_empty;
	wire [1-1:0]	tx_full;
	wire [1-1:0]	tx_level_below;
	wire [MDW-1:0]	rdata;
	wire [1-1:0]	rx_empty;
	wire [1-1:0]	rx_full;
	wire [1-1:0]	rx_level_above;
	wire [1-1:0]	break_flag;
	wire [1-1:0]	match_flag;
	wire [1-1:0]	frame_error_flag;
	wire [1-1:0]	parity_error_flag;
	wire [1-1:0]	overrun_flag;
	wire [1-1:0]	timeout_flag;


	wire	[MDW-1:0]	rxdata_WIRE;

	wire	[MDW-1:0]	txdata_WIRE;

	reg [16-1:0]	prescaler_REG;
	assign	prescaler = prescaler_REG;
	always @(posedge PCLK or negedge PRESETn) if(~PRESETn) prescaler_REG <= 0;
                                        else if(apb_we & (PADDR[16-1:0]==prescaler_REG_OFFSET))
                                            prescaler_REG <= PWDATA[16-1:0];

	reg [5-1:0]	control_REG;
	assign	en	=	control_REG[0 : 0];
	assign	tx_en	=	control_REG[1 : 1];
	assign	rx_en	=	control_REG[2 : 2];
	assign	loopback_en	=	control_REG[3 : 3];
	assign	glitch_filter_en	=	control_REG[4 : 4];
	always @(posedge PCLK or negedge PRESETn) if(~PRESETn) control_REG <= 0;
                                        else if(apb_we & (PADDR[16-1:0]==control_REG_OFFSET))
                                            control_REG <= PWDATA[5-1:0];

	reg [14-1:0]	config_REG;
	assign	data_size	=	config_REG[3 : 0];
	assign	stop_bits_count	=	config_REG[4 : 4];
	assign	parity_type	=	config_REG[7 : 5];
	assign	timeout_bits	=	config_REG[13 : 8];
	always @(posedge PCLK or negedge PRESETn) if(~PRESETn) config_REG <= 'h3F08;
                                        else if(apb_we & (PADDR[16-1:0]==config_REG_OFFSET))
                                            config_REG <= PWDATA[14-1:0];

	reg [16-1:0]	fifo_control_REG;
	assign	txfifotr	=	fifo_control_REG[(FAW - 1) : 0];
	assign	rxfifotr	=	fifo_control_REG[(FAW + 7) : 8];
	always @(posedge PCLK or negedge PRESETn) if(~PRESETn) fifo_control_REG <= 0;
                                        else if(apb_we & (PADDR[16-1:0]==fifo_control_REG_OFFSET))
                                            fifo_control_REG <= PWDATA[16-1:0];

	wire [16-1:0]	fifo_status_WIRE;
	assign	fifo_status_WIRE[(FAW - 1) : 0] = rx_level;
	assign	fifo_status_WIRE[(FAW + 7) : 8] = tx_level;

	reg [MDW-1:0]	match_REG;
	assign	match_data = match_REG;
	always @(posedge PCLK or negedge PRESETn) if(~PRESETn) match_REG <= 0;
                                        else if(apb_we & (PADDR[16-1:0]==match_REG_OFFSET))
                                            match_REG <= PWDATA[MDW-1:0];

	reg [9:0] IM_REG;
	reg [9:0] IC_REG;
	reg [9:0] RIS_REG;

	wire[10-1:0]      MIS_REG	= RIS_REG & IM_REG;
	always @(posedge PCLK or negedge PRESETn) if(~PRESETn) IM_REG <= 0;
                                        else if(apb_we & (PADDR[16-1:0]==IM_REG_OFFSET))
                                            IM_REG <= PWDATA[10-1:0];
	always @(posedge PCLK or negedge PRESETn) if(~PRESETn) IC_REG <= 10'b0;
                                        else if(apb_we & (PADDR[16-1:0]==IC_REG_OFFSET))
                                            IC_REG <= PWDATA[10-1:0];
                                        else
                                            IC_REG <= 10'd0;

	wire [0:0] break = break_flag;
	wire [0:0] match = match_flag;
	wire [0:0] frame_error = frame_error_flag;
	wire [0:0] parity_error = parity_error_flag;
	wire [0:0] overrun = overrun_flag;
	wire [0:0] timeout = timeout_flag;


	integer _i_;
	always @(posedge PCLK or negedge PRESETn) if(~PRESETn) RIS_REG <= 0; else begin
		for(_i_ = 0; _i_ < 1; _i_ = _i_ + 1) begin
			if(IC_REG[_i_]) RIS_REG[_i_] <= 1'b0; else if(tx_empty[_i_ - 0] == 1'b1) RIS_REG[_i_] <= 1'b1;
		end
		for(_i_ = 1; _i_ < 2; _i_ = _i_ + 1) begin
			if(IC_REG[_i_]) RIS_REG[_i_] <= 1'b0; else if(rx_full[_i_ - 1] == 1'b1) RIS_REG[_i_] <= 1'b1;
		end
		for(_i_ = 2; _i_ < 3; _i_ = _i_ + 1) begin
			if(IC_REG[_i_]) RIS_REG[_i_] <= 1'b0; else if(tx_level_below[_i_ - 2] == 1'b1) RIS_REG[_i_] <= 1'b1;
		end
		for(_i_ = 3; _i_ < 4; _i_ = _i_ + 1) begin
			if(IC_REG[_i_]) RIS_REG[_i_] <= 1'b0; else if(rx_level_above[_i_ - 3] == 1'b1) RIS_REG[_i_] <= 1'b1;
		end
		for(_i_ = 4; _i_ < 5; _i_ = _i_ + 1) begin
			if(IC_REG[_i_]) RIS_REG[_i_] <= 1'b0; else if(break[_i_ - 4] == 1'b1) RIS_REG[_i_] <= 1'b1;
		end
		for(_i_ = 5; _i_ < 6; _i_ = _i_ + 1) begin
			if(IC_REG[_i_]) RIS_REG[_i_] <= 1'b0; else if(match[_i_ - 5] == 1'b1) RIS_REG[_i_] <= 1'b1;
		end
		for(_i_ = 6; _i_ < 7; _i_ = _i_ + 1) begin
			if(IC_REG[_i_]) RIS_REG[_i_] <= 1'b0; else if(frame_error[_i_ - 6] == 1'b1) RIS_REG[_i_] <= 1'b1;
		end
		for(_i_ = 7; _i_ < 8; _i_ = _i_ + 1) begin
			if(IC_REG[_i_]) RIS_REG[_i_] <= 1'b0; else if(parity_error[_i_ - 7] == 1'b1) RIS_REG[_i_] <= 1'b1;
		end
		for(_i_ = 8; _i_ < 9; _i_ = _i_ + 1) begin
			if(IC_REG[_i_]) RIS_REG[_i_] <= 1'b0; else if(overrun[_i_ - 8] == 1'b1) RIS_REG[_i_] <= 1'b1;
		end
		for(_i_ = 9; _i_ < 10; _i_ = _i_ + 1) begin
			if(IC_REG[_i_]) RIS_REG[_i_] <= 1'b0; else if(timeout[_i_ - 9] == 1'b1) RIS_REG[_i_] <= 1'b1;
		end
	end

	assign IRQ = |MIS_REG;

	AUCOHL_UART #(
		.SC(SC),
		.MDW(MDW),
		.GFLEN(GFLEN),
		.FAW(FAW)
	) instance_to_wrap (
		.clk(clk),
		.rst_n(rst_n),
		.prescaler(prescaler),
		.en(en),
		.tx_en(tx_en),
		.rx_en(rx_en),
		.wdata(wdata),
		.timeout_bits(timeout_bits),
		.loopback_en(loopback_en),
		.glitch_filter_en(glitch_filter_en),
		.tx_level(tx_level),
		.rx_level(rx_level),
		.rd(rd),
		.wr(wr),
		.data_size(data_size),
		.stop_bits_count(stop_bits_count),
		.parity_type(parity_type),
		.txfifotr(txfifotr),
		.rxfifotr(rxfifotr),
		.match_data(match_data),
		.tx_empty(tx_empty),
		.tx_full(tx_full),
		.tx_level_below(tx_level_below),
		.rdata(rdata),
		.rx_empty(rx_empty),
		.rx_full(rx_full),
		.rx_level_above(rx_level_above),
		.break_flag(break_flag),
		.match_flag(match_flag),
		.frame_error_flag(frame_error_flag),
		.parity_error_flag(parity_error_flag),
		.overrun_flag(overrun_flag),
		.timeout_flag(timeout_flag),
		.rx(rx),
		.tx(tx)
	);

	assign	PRDATA = 
			(PADDR[16-1:0] == rxdata_REG_OFFSET)	? rxdata_WIRE :
			(PADDR[16-1:0] == txdata_REG_OFFSET)	? txdata_WIRE :
			(PADDR[16-1:0] == prescaler_REG_OFFSET)	? prescaler_REG :
			(PADDR[16-1:0] == control_REG_OFFSET)	? control_REG :
			(PADDR[16-1:0] == config_REG_OFFSET)	? config_REG :
			(PADDR[16-1:0] == fifo_control_REG_OFFSET)	? fifo_control_REG :
			(PADDR[16-1:0] == fifo_status_REG_OFFSET)	? fifo_status_WIRE :
			(PADDR[16-1:0] == match_REG_OFFSET)	? match_REG :
			(PADDR[16-1:0] == IM_REG_OFFSET)	? IM_REG :
			(PADDR[16-1:0] == MIS_REG_OFFSET)	? MIS_REG :
			(PADDR[16-1:0] == RIS_REG_OFFSET)	? RIS_REG :
			(PADDR[16-1:0] == IC_REG_OFFSET)	? IC_REG :
			32'hDEADBEEF;

	assign PREADY = 1'b1;

	assign	rxdata_WIRE = rdata;
	assign	rd = (apb_re & (PADDR[16-1:0] == rxdata_REG_OFFSET));
	assign	wdata = txdata_WIRE;
	assign	wr = (apb_we & (PADDR[16-1:0] == txdata_REG_OFFSET));
endmodule
/*
	Copyright 2022 AUCOHL

	Author: Mohamed Shalan (mshalan@aucegypt.edu)

	Permission is hereby granted, free of charge, to any person obtaining
	a copy of this software and associated documentation files (the
	"Software"), to deal in the Software without restriction, including
	without limitation the rights to use, copy, modify, merge, publish,
	distribute, sublicense, and/or sell copies of the Software, and to
	permit persons to whom the Software is furnished to do so, subject to
	the following conditions:

	The above copyright notice and this permission notice shall be
	included in all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
	NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
	LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
	OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
	WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/

/* THIS FILE IS GENERATED, edit it to complete the testbench */

`timescale		1ns/1ps

`default_nettype	none




/*
	Copyright (C) 2020 AUCOHL
    
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














































                                                



module AUCOHL_UART_APB_tb;

	// Change the following parameters as desired
	parameter real CLOCK_PERIOD = 100.0;
	parameter real RESET_DURATION = 999.0;

	// DON NOT Change the following parameters
	localparam [16-1:0]
				RXDATA_REG_OFFSET =     16'h0000,
				TXDATA_REG_OFFSET =     16'h0004,
				PRESCALER_REG_OFFSET =  16'h000c,
				CONTROL_REG_OFFSET =    16'h0008,
				CONFIG_REG_OFFSET =     16'h0010,
				FIFO_CONTROL_REG_OFFSET =       16'h0014,
				FIFO_STATUS_REG_OFFSET =        16'h0018,
				MATCH_REG_OFFSET =      16'h001c,
				IM_REG_OFFSET =         16'h0f00,
				IC_REG_OFFSET =         16'h0f0c,
				RIS_REG_OFFSET =        16'h0f08,
				MIS_REG_OFFSET =        16'h0f04;

	reg PWRITE, PENABLE, PSEL; reg [31:0] PWDATA, PADDR; wire PREADY; wire [31:0] PRDATA; wire IRQ;

	wire	[0:0]	rx;
	wire	[0:0]	tx;

	reg				vip_tx;
	wire			vip_rx;

	assign 			rx 		= vip_tx;
	assign 			vip_rx 	= tx;

	reg PCLK=0; always #(CLOCK_PERIOD/2) PCLK = !PCLK;
	reg PRESETn=1'bx;
                                                event e_assert_reset, e_reset_done;
                                                initial forever begin
                                                    @(e_assert_reset);
                                                    PRESETn = 1'b0;
                                                    #RESET_DURATION;
                                                    @(posedge PCLK) PRESETn = ~1'b0;
                                                    -> e_reset_done;
                                                end
	initial begin $dumpfile("APB_AUCOHL_UART_tb.vcd"); $dumpvars(0, AUCOHL_UART_APB_tb); end
	initial begin #1_000_000; $display("Verification Failed: Timeout");$finish; end

	AUCOHL_UART_APB DUV (
		.PCLK(PCLK),
                                                .PRESETn(PRESETn),
                                                .PWRITE(PWRITE),
                                                .PWDATA(PWDATA),
                                                .PADDR(PADDR),
                                                .PENABLE(PENABLE),
                                                .PSEL(PSEL),
                                                .PREADY(PREADY),
                                                .PRDATA(PRDATA),
                                                .IRQ(IRQ),
		.rx(rx),
		.tx(tx)
	);

/*
	Copyright (C) 2020 AUCOH
    
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

task APB_W_WRITE (input [31:0] address, input [31:0] data );
    begin
        @(posedge PCLK);
        PSEL    = 1;
        PWRITE  = 1;
        PWDATA  = data;
        PENABLE = 0;
        PADDR   = address;
        @(posedge PCLK);
        PENABLE = 1;
        @(posedge PCLK);
        PSEL    = 0;
        PWRITE  = 0;
        PENABLE = 0;
    end
endtask
		
task APB_W_READ(input [31:0] address, output [31:0] data );
    begin
        @(posedge PCLK);
        PSEL    = 1;
        PWRITE  = 0;
        PENABLE = 0;
        PADDR   = address;
        @(posedge PCLK);
        PENABLE = 1;
        @(posedge PCLK);
        wait(PREADY == 1)
        data    = PRDATA;
        PSEL    = 0;
        PWRITE  = 0;
        PENABLE = 0;
    end
endtask
task send_serial_8N1(   input real bit_duration, 
                        input [7:0] data); 
    begin : send_serial_8N1_body
        integer i;
        vip_tx = 1'b0;
        #bit_duration;
        for(i=0; i<8; i=i+1) begin
            vip_tx = data[i];
            #bit_duration;
        end
        vip_tx = 1'b1;
        #bit_duration;
    end
endtask	

	event	e_test1_start, e_test1_done;
	event	e_test2_start, e_test2_done;
	
	localparam real BIT_TIME_115200 = 8680.55,
					BIT_TIME_57600 = 17361.11;
	initial begin
		#999 -> e_assert_reset;
		@(e_reset_done);

		// Do some initializations
		// Set the baud rate to 115,200 bps
		// 0.1152 = 10/((PR+1)*8) ==> PR = 9.85 (~10)
		// Actual baud = 113.636 baud/s
		// bit time = 8.6805 us (actual: 8.8 us)
		APB_W_WRITE(PRESCALER_REG_OFFSET, 16'd10);
		// Configure the line for 8N1
		APB_W_WRITE(CONFIG_REG_OFFSET, 32'b111111_000_0_1000);
		// Perform Test 1
		#1000 -> e_test1_start;
		@(e_test1_done);

		#1000 -> e_test2_start;
		@(e_test2_done);

		// Finish the simulation
		$display("All tests have passed");
		#1000 $finish();
	end

	// Serial Send Thread
	event e_send_serial_data;
	initial begin
		forever begin
			@(e_send_serial_data) send_serial_8N1(BIT_TIME_115200, 8'hA5);	
		end
	end

	reg [31:0] prdata;
	// Test 1 -- Receive an 8N1 frame @ 115000 baud rate
	initial  begin : test1
                                                @(e_test1_start);
		// Test 1 code goes here
		// Enable the receiver and the UART
		APB_W_WRITE(CONTROL_REG_OFFSET, 32'b0_0_1_0_1);
		//send_serial_8N1(8696, 8'hA5);
		->e_send_serial_data;
		forever begin
			APB_W_READ(RIS_REG_OFFSET, prdata);
			if((prdata & 32'h8) != 0) begin
				#1_000;
				APB_W_READ(RXDATA_REG_OFFSET, prdata);
				#1_1000;
				if(prdata == 'hA5) begin
					break;
					#1;
				end else begin
					$display("Received Wrong data %x vs. %x", prdata, 'hA5);
					$finish;
				end
			end
		end
	-> e_test1_done;
                                                end	
	
	// Test 2 -- Send a frame @ 57600
	initial  begin : test2
                                                @(e_test2_start);
		// Disable the UART fully
		APB_W_WRITE(CONTROL_REG_OFFSET, 32'b0);
		// Configure the prescaler for 57600
		// Prescaler has to be 20.7 
		// will go for 21 which gives 56818.18 baud/s
		APB_W_WRITE(PRESCALER_REG_OFFSET, 16'd21);
		// Configure the line for 812
		APB_W_WRITE(CONFIG_REG_OFFSET, 32'b111111_101_1_1000);
		// Enable the transmission
		APB_W_WRITE(CONTROL_REG_OFFSET, 32'b0_0_0_1_1);
		// Send two different frames
		APB_W_WRITE(RXDATA_REG_OFFSET, 'hC3);
		APB_W_WRITE(RXDATA_REG_OFFSET, 'h91);
		// Wait for the time of two frames = 2 x 11 x 17.6 = 387.2 usec
		#387_200;
		// This test is a manual one. Check the waveform!
	-> e_test2_done;
                                                end
endmodule
