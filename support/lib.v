(* blackbox *)
module SEER(A, Y);
parameter OFFSET = 0;
parameter WIDTH = 0;
input [WIDTH-1:0] A;
output [WIDTH-1:0] Y;
endmodule

(* blackbox *)
module IMPORT(D, Q);
parameter WIDTH = 0;
parameter ZEROED = 0;
parameter NAMESPACE = "";
parameter FROM_NODE = "";
parameter TO_NODE = "";
parameter STALK = "";
input [WIDTH-1:0] D;
output [WIDTH-1:0] Q;
endmodule

(* blackbox *)
module VAR_SET(D, EN);
parameter WIDTH = 0;
parameter AT_NODE = "";
parameter NAMESPACE = "";
parameter NAME = "";
input [WIDTH-1:0] D;
input EN;
endmodule

(* blackbox *)
module VAR_GET(Q);
parameter WIDTH = 0;
parameter AT_NODE = "";
parameter NAMESPACE = "";
parameter NAME = "";
output [WIDTH-1:0] Q;
endmodule

(* blackbox *)
module TIMEPORTAL(A, AY, B, BY);
parameter A_WIDTH = 0;
parameter B_WIDTH = 0;
input [A_WIDTH-1:0] A;
output [A_WIDTH-1:0] AY;
input [B_WIDTH-1:0] B;
output [B_WIDTH-1:0] BY;
endmodule

(* blackbox *)
module MUTEX_ASSERT(CLK, A);
parameter WIDTH = 0;
parameter MESSAGE = "";
input wire CLK;
input [WIDTH-1:0] A;
endmodule

(* blackbox *)
module BACKEDGE(A, AY, B, BY);
parameter A_WIDTH = 0;
parameter B_WIDTH = 0;
input [A_WIDTH-1:0] A;
output [A_WIDTH-1:0] AY;
input [B_WIDTH-1:0] B;
output [B_WIDTH-1:0] BY;
endmodule

(* blackbox *)
module ORACLE(Y);
parameter WIDTH = 0;
output [WIDTH-1:0] Y;
endmodule

(* blackbox *)
module EXECID_INC(A, Y, EN);
parameter WIDTH = 0;
input [WIDTH-1:0] A;
output [WIDTH-1:0] Y;
input EN;
endmodule

(* blackbox *)
module EXECID_DESCEND(A, Y, EN);
parameter WIDTH = 0;
input [WIDTH-1:0] A;
output [WIDTH-1:0] Y;
input EN;
endmodule

(* blackbox *)
module EXECID_ASCEND(A, Y, EN);
parameter WIDTH = 0;
input [WIDTH-1:0] A;
output [WIDTH-1:0] Y;
input EN;
endmodule

(* blackbox *)
module EXECID_GT(A, B, Y, EN);
parameter WIDTH = 0;
input [WIDTH-1:0] A;
input [WIDTH-1:0] B;
output Y;
input EN;
endmodule

(* blackbox *)
module EXECID_GE(A, B, Y, EN);
parameter WIDTH = 0;
input [WIDTH-1:0] A;
input [WIDTH-1:0] B;
output Y;
input EN;
endmodule

(* blackbox *)
module EXECID_EQ(A, B, Y, EN);
parameter WIDTH = 0;
input [WIDTH-1:0] A;
input [WIDTH-1:0] B;
output Y;
input EN;
endmodule

(* blackbox *)
module EXECID_ZERO(Y, EN);
parameter WIDTH = 0;
output [WIDTH-1:0] Y;
input EN;
endmodule

(* blackbox *)
module EXECID_MAX(Y, EN, A);
parameter WIDTH = 0;
parameter Y_WIDTH = 0;
output [Y_WIDTH-1:0] Y;
input [WIDTH-1:0] A;
input [(Y_WIDTH/WIDTH)-1:0] EN;
endmodule

(* blackbox *)
module EXECID_FORK(A, Y, EN);
parameter WIDTH = 0;
input [WIDTH-1:0] A;
output [WIDTH-1:0] Y;
input EN;
endmodule
