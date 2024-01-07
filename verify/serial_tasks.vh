task send_serial_8N1(   input real bit_duration, 
                        input integer i,
                        input [7:0] data); 
    begin
        tx = 1'b0;
        #bit_duration;
        for(i=0; i<8; i=i+1) begin
            tx = data[i];
            #bit_duration;
        end
        tx = 1'b1;
        #bit_duration;
    end
endtask