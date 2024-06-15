# Grammar

$$
\begin{align}
    \text{[Prog]} &\to \text{[stmt]}^* \\
    \text{[stmt]} &\to
        \begin{cases}
        return \text{ [Expr];} \\
        \text{[type]} \text{ [ident]} = \text{[Expr]}; \\
        \text{[scope]} \\
        \text{[if]} \\
        \text{[ident] = [Expr];} \\
        \end{cases} \\
    \text{[scope]} &\to [stmt]^*\\
    \text{[return]} &\to return\text{ [Expr]}; \\
    \text{[if]} &\to \text{([LogicExpr])} => [stmt]^* \\
    \text{[Expr]} &\to
    \begin{cases}
        \text{[Term]} \\
        \text{[BinExpr]}\\
        \text{[LogicExpr]}\\
        \text{[CompExpr]}\\
    \end{cases} \\
    \text{[BinExpr]} &\to
    \begin{cases}
        \text{[Expr] [binary operator] [Expr]}
    \end{cases}\\
    \text{[LogicExpr]} &\to
    \begin{cases}
        \text{[Expr] [logic operator] [Expr]}
    \end{cases} \\
    \text{[CompExpr]} &\to
    \begin{cases}
        \text{[Term] [comparison operator] [Term]}
    \end{cases} \\
    \text{[Term]} &\to
    \begin{cases}
        \text{[ident]} \\
        \text{immediate int} \\
        \text{([Expr])}\\
    \end{cases} \\
    \text{[binary operator]} &\to
    \begin{cases}
        \text{*} & \text{prec = 1} \\
        \text{/} & \text{prec = 1} \\
        \text{+} & \text{prec = 0} \\
        \text{-} & \text{prec = 0} \\
    \end{cases} \\
    \text{[logic operator]} &\to
    \begin{cases}
        \text{\&\&} & \text{(AND)} & \text{prec = 1} \\
        \text{||} & \text{(OR)} & \text{prec = 0} \\
        \text{!} & \text{(NOT)} & \text{prec = 0} \\
    \end{cases}\\
    \text{[comparison operator]} &\to
    \begin{cases}
        \text{==} & \text{(EQUALS)} \\
        \text{>} & \text{(STRICTLY GREATER)} \\
        \text{>=} & \text{(GREATER OR EQUAL)}\\
        \text{<} & \text{(STRICTLY LOWER)} \\
        \text{<=} & \text{(LOWER OR EQUAL)} \\
        \text{!=} & \text{(NOT EQUALS)}
    \end{cases} \\
    \text{[ident]} &\to \text{variable name} \\
    \text{[type]} &\to
        \begin{cases}
            \text{int} \\
            \text{bool}
        \end{cases} \\
    \text{} \\
\end{align}
$$

For information on the types, see syntax.
