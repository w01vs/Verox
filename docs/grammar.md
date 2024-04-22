# Grammar
$$ 
\begin{align}
    \text{[Prog]} &\to \text{[stmt]}^* \\
    \text{[stmt]} &\to 
        \begin{cases}
        return \text{ [Expr];} \\
        print(\text{[Expr]}); \\
        type \text{ ident} = \text{[Expr]};
        \end{cases} \\
    \text{[return]} &\to return\text{ [Expr]}; 
    \\
    \text{[Expr]} &\to 
    \begin{cases}
        \text{Term} \\
        \text{[BinExpr]}\\
    \end{cases} \\
    \text{[BinExpr]} &\to
    \begin{cases}
        \text{[Expr] [operator] [Expr]}
    \end{cases}\\
    \text{[Term]} &\to
    \begin{cases}
        \text{ident} \\
        \text{immediate int} \\
        \text{([Expr])}\\
    \end{cases} \\
    \text{[operator]} &\to
    \begin{cases}
        \text{*} & \text{prec = 1} \\
        \text{/} & \text{prec = 1} \\
        \text{+} & \text{prec = 0} \\
        \text{-} & \text{prec = 0} \\
    \end{cases}\\
    \text{[type]} &\to 
        \begin{cases}
            \text{int}
        \end{cases}\\
    \text{}\\
\end{align}
$$

For information on the types, see syntax.\
Print can only be called once at this time.
