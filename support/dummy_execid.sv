module EXECID_ZERO(Y, EN);
parameter WIDTH = 0;
output [WIDTH-1:0] Y;
assign Y = 0;
input EN;
endmodule

module EXECID_INC(A, Y, EN);
parameter WIDTH = 0;
localparam SLOTBITS = 8;
input [WIDTH-1:0] A;
output [WIDTH-1:0] Y;
input EN;

wire [SLOTBITS-1:0] inc;
integer slot;
integer i;

always @(*) begin
	slot = -1;
	for (i = WIDTH / SLOTBITS - 1; i >= 0; i = i - 1) begin
		if (A[i*SLOTBITS+:SLOTBITS] != 0)
			slot = i;
	end

	if (EN) assert (slot != -1);
	Y = A;
	inc = A[slot * SLOTBITS+:SLOTBITS] + 1;
	if (EN) assert(inc != 0);
	Y[slot * SLOTBITS+:SLOTBITS] = inc;
end
endmodule

module EXECID_DESCEND(A, Y, EN);
parameter WIDTH = 0;
localparam SLOTBITS = 8;
input [WIDTH-1:0] A;
output [WIDTH-1:0] Y;
input EN;

wire [SLOTBITS-1:0] inc;
integer slot;
integer i;

always @(*) begin
	slot = -1;
	for (i = 0; i < WIDTH / SLOTBITS; i = i + 1) begin
		if (A[i*SLOTBITS+:SLOTBITS] == 0)
			slot = i;
	end

	if (EN)
		assert (slot != -1);
	Y = A;
	Y[slot * SLOTBITS+:SLOTBITS] = 1;
end
endmodule

module EXECID_ASCEND(A, Y, EN);
parameter WIDTH = 0;
localparam SLOTBITS = 8;
input [WIDTH-1:0] A;
output [WIDTH-1:0] Y;
input EN;

wire [SLOTBITS-1:0] inc;
integer slot;
integer i;

always @(*) begin
	slot = -1;
	for (i = WIDTH / SLOTBITS - 1; i >= 0; i = i - 1) begin
		if (A[i*SLOTBITS+:SLOTBITS] != 0)
			slot = i;
	end

	if (EN) begin
		assert (slot != -1);
		assert (Y[(slot + 1) * SLOTBITS+:SLOTBITS] < (1 << SLOTBITS) - 1);
	end
	Y = A;
	Y[(slot + 1) * SLOTBITS+:SLOTBITS] += 1;
	Y[slot * SLOTBITS+:SLOTBITS] = 0;
end
endmodule

module EXECID_GT(A, B, Y, EN);
parameter WIDTH = 0;
input [WIDTH-1:0] A;
input [WIDTH-1:0] B;
output Y;
assign Y = A > B;
input EN;
endmodule

module EXECID_GE(A, B, Y, EN);
parameter WIDTH = 0;
input [WIDTH-1:0] A;
input [WIDTH-1:0] B;
output Y;
assign Y = A >= B;
input EN;
endmodule

module EXECID_EQ(A, B, Y, EN);
parameter WIDTH = 0;
input [WIDTH-1:0] A;
input [WIDTH-1:0] B;
output Y;
assign Y = A == B;
input EN;
endmodule

module EXECID_MAX(Y, EN, A);
parameter WIDTH = 0;
parameter Y_WIDTH = 0;
output [Y_WIDTH-1:0] Y;
input [WIDTH-1:0] A;
input [(WIDTH/Y_WIDTH)-1:0] EN;
wire [Y_WIDTH-1:0] A_part;

integer i;
always @(*) begin
	Y = 0;
	for (i = 0; i < (WIDTH / Y_WIDTH); i = i + 1) begin
		A_part = A[Y_WIDTH*i+:Y_WIDTH];
		if (EN[i] && A_part > Y)
			Y = A_part;
	end
end
endmodule

module EXECID_FORK(A, Y, EN);
parameter WIDTH = 0;
input [WIDTH-1:0] A;
output Y;
input EN;
always @(*) begin
	$display("Warning: Design contains forks, those are unsupported with dummy_execid.");
	assert(!EN); // fork not supported in dummy execid
end
endmodule
