# Grammar
$$ 
\begin{align}
    \text{[prog]} &\to \text{[stmt]}^* \\
    \text{[stmt]} &\to 
        \begin{cases}
        return \text{ [expr];} \\
        type \text{ ident} = \text{[expr]};
        \end{cases} \\
    \text{[return]} &\to return\text{ [expr]}; 
    \\
    \text{[expr]} &\to 
    \begin{cases}
        \text{i\_int} \\
        \text{ident}
    \end{cases} \\
    \text{[type]} &\to 
        \begin{cases}
            \text{int}
        \end{cases}
\end{align} \\
    \text{For information on the types, see syntax.}
$$