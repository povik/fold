module MUTEX_ASSERT (CLK, A);
parameter WIDTH = 0;
parameter MESSAGE = "";
input wire CLK;
input wire [WIDTH-1:0] A;

reg [0:0] ignore = 1'b1;

always @(posedge CLK)
	ignore <= 1'b0;

always @(posedge CLK)
	assert((((A-1) & A) == 0) || ignore);

endmodule
