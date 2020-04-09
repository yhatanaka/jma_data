```flow

st=>start: Start
op=>operation: ヘッダ行数決定
cond=>condition: Yes or No?
e=>end

st->op->cond
cond(yes)->e
cond(no)->op
```

