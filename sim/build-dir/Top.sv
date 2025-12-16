module Top (
    input  wire        clock,
    input  wire        reset,
    input  wire [7:0]  io_in,
    output wire [7:0]  io_out
);
    reg [7:0] counter;

    always @(posedge clock) begin
        if (reset) begin
            counter <= 8'h0;
        end else begin
            counter <= counter + io_in;
        end
    end

    assign io_out = counter;
endmodule
