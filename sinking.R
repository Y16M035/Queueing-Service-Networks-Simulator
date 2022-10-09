ARRIVAL
lambda: 2
transitions: 1-1

NODE
id: 1
arrival: M
exit: M
servers: 3
queue discipline: LIFO
capacity: 123
mu: 2
queue cost per client: 34.5
service cost per client: 21
infrastructure cost: 200
service revenue per client: 50
transitions: 1-0.2, 2-0.2, 3-0.2, 4-0.2

NODE
id: 2
arrival: M
exit: M
servers: 3
queue discipline: FIFO
capacity: 123
mu: 2
queue cost per client: 34.5
service cost per client: 21
infrastructure cost: 200
service revenue per client: 50
transitions: 1-0.2, 2-0.2, 3-0.2, 4-0.2

NODE
id: 3
arrival: M
exit: M
servers: 3
queue discipline: RSS
capacity: 123
mu: 2
queue cost per client: 34.5
service cost per client: 21
infrastructure cost: 200
service revenue per client: 50
transitions: 1-0.2, 2-0.2, 3-0.2, 4-0.2

NODE
id: 4
arrival: M
exit: M
servers: 3
queue discipline: PR-5
capacity: 123
mu: 2
queue cost per client: 34.5
service cost per client: 21
infrastructure cost: 200
service revenue per client: 50
transitions: 1-0.2, 2-0.2, 3-0.2, 4-0.2
