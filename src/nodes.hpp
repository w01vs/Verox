#pragma once

#include "token.hpp"
#include <variant>
#include <vector>
#include "type.hpp"

struct NodeIntLit {
    Token i_int; // immediate int
};

struct NodeIdent {
    Token ident; // var name
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
    std::optional<Type> type;
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

struct NodeLogicExprEq {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeLogicExpr {
    std::variant<NodeLogicExprAnd*, NodeLogicExprOr*, NodeLogicExprNot*, NodeLogicExprEq*> val;
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

// internal 'functions' such as return and possibly print etc.
struct NodeInternal {
    std::variant<NodeInternalRet*, NodeInternalPrintf*> ret; // return
};

// variable declaration with a type.
struct NodeStmtVar {
    Token ident; // var name
    Type type;
    NodeExpr* expr; // var value
};

struct NodeScope;

struct NodeStmt {
    std::variant<NodeInternal*, NodeStmtVar*, NodeScope*> var; // Internal stuff or variable
};

struct NodeScope {
    std::vector<NodeStmt*> stmts; // All statements in a scope
};

struct NodeIf {
    NodeExpr* cond;
    NodeScope* scope;
};

struct NodeProg {
    std::vector<NodeStmt*> stmts; // All statements outside scopes
};