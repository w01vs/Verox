#pragma once

#include "tokens.hpp"
#include <variant>
#include <vector>
#include "type.hpp"

struct NodeIntLit {
    Token i_int; // immediate int
};

struct NodeIdent {
    Token ident; // var name
    std::string structname;
};

struct NodeBool {
    Token boolean; // boolean value
};

// Forward declaration
struct NodeTermParens;

struct NodeBinTerm {
    std::variant<NodeIdent*, NodeIntLit*, NodeTermParens*> val;
};

struct NodeCompExpr; 

struct NodeLogicTerm {
    std::variant<NodeIdent*, NodeBool*, NodeTermParens*, NodeCompExpr*> val;
};

// Forward declarations
struct NodeBinExpr;
struct NodeLogicExpr;

struct NodeCompExprEq {
    NodeBinTerm* lhs;
    NodeBinTerm* rhs;
};

struct NodeCompExprGreater {
    NodeBinTerm* lhs;
    NodeBinTerm* rhs;
};

struct NodeCompExprLess {
    NodeBinTerm* lhs;
    NodeBinTerm* rhs;
};

struct NodeCompExprGreaterEq {
    NodeBinTerm* lhs;
    NodeBinTerm* rhs;
};

struct NodeCompExprLessEq {
    NodeBinTerm* lhs;
    NodeBinTerm* rhs;
};

struct NodeCompExprNeq {
    NodeBinTerm* lhs;
    NodeBinTerm* rhs;
};

struct NodeCompExpr {
    std::variant<NodeCompExprEq*, NodeCompExprGreater*, NodeCompExprLess*, NodeCompExprGreaterEq*, NodeCompExprLessEq*, NodeCompExprNeq*> val;
};

struct NodeExpr {
    std::variant<NodeBinTerm*, NodeBinExpr*, NodeLogicTerm*, NodeLogicExpr*, NodeCompExpr*> var;
    std::optional<UDType> type;
};

struct NodeLogicExprAnd {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeLogicExprOr {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeLogicExprNot {
    NodeExpr* expr;
};

struct NodeLogicExpr {
    std::variant<NodeLogicExprAnd*, NodeLogicExprOr*, NodeLogicExprNot*> val;
};

struct NodeTermParens {
    NodeExpr* expr;
};

struct NodeBinExprAdd {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeBinExprMult {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeBinExprSub {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeBinExprDiv {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeBinExpr {
    std::variant<NodeBinExprAdd*, NodeBinExprDiv*, NodeBinExprSub*, NodeBinExprMult*> val;
};

struct NodeInternalRet {
    NodeExpr* ret; // return val
};

struct NodeInternalPrintf {
    NodeExpr* print;
};

enum class NodeLoopFlow : char {
    CONTINUE = 0,
    BREAK = 1,
};

// internal functions (return, print) and functional keywords (continue, break)
struct NodeInternal {
    std::variant<NodeInternalRet*, NodeInternalPrintf*, NodeLoopFlow*> internal; // internals
};

struct NodeStmtStructMove {
    UDType type;
    std::vector<std::variant<NodeExpr*, NodeStmtStructMove*>> exprs;
};

struct NodeStmtStruct {
    Token ident;
    NodeStmtStructMove* init;
};

// variable declaration with a type.
struct NodeStmtVar {
    Token ident; // var name
    UDType type;
    NodeExpr* expr; // var value
};

struct NodeScope;
struct NodeIf;


struct NodeStmtAssign {
    Token ident; // var name
    NodeExpr* expr; // var value
};

struct NodeWhile;

struct NodeStmt {
    std::variant<NodeInternal*, NodeStmtVar*, NodeScope*, NodeIf*, NodeStmtAssign*, NodeWhile*, NodeStmtStruct*, bool> var; // Internal stuff or variable
};

struct NodeScope {
    std::vector<NodeStmt*> stmts; // All statements in a scope
};

struct NodeWhile {
    NodeExpr* cond;
    NodeScope* scope;
};

struct NodeIf {
    NodeExpr* cond;
    NodeScope* scope;
    std::vector<NodeIf*> elseif_stmts;
    std::optional<NodeScope*> else_stmts;
};

struct NodeProg {
    std::vector<NodeStmt*> stmts; // All statements outside scopes
};