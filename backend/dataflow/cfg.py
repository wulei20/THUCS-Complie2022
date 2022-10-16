from backend.dataflow.basicblock import BasicBlock

"""
CFG: Control Flow Graph

nodes: sequence of basicblock
edges: sequence of edge(u,v), which represents after block u is executed, block v may be executed
links: links[u][0] represent the Prev of u, links[u][1] represent the Succ of u,
"""


class CFG:
    def __init__(self, nodes: list[BasicBlock], edges: list[(int, int)]) -> None:
        self.nodes = nodes                      # 点信息
        self.edges = edges                      # 边信息

        self.links = []

        for i in range(len(nodes)):
            self.links.append((set(), set()))   # 控制流图上每个节点的前驱、后继集合

        for (u, v) in edges:
            self.links[u][1].add(v)
            self.links[v][0].add(u)
        
        self.reachable = [0 for i in range(len(nodes))]
        self.reachchild(0)

    def getBlock(self, id):
        return self.nodes[id]

    def getPrev(self, id):
        return self.links[id][0]

    def getSucc(self, id):
        return self.links[id][1]

    def getInDegree(self, id):
        return len(self.links[id][0])

    def getOutDegree(self, id):
        return len(self.links[id][1])

    def iterator(self):
        return iter(self.nodes)
    
    """
    dfs算法从一个可达节点访问其所有可达节点，并将它们的状态修改为可达的
    """
    def reachchild(self, id):
        if self.unreachable(id):
            self.reachable[id] = 1
            for _id in self.getSucc(id):
                self.reachchild(_id)
        return

    """
    返回一个节点是否可达
    """
    def unreachable(self, id):
        return not self.reachable[id]
