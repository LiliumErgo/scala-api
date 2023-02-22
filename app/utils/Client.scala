package utils

import configs.serviceOwnerConf
import node.BaseClient

class Client()
    extends BaseClient(
      nodeInfo = utils.DefaultNodeInfo(
        new network(
          serviceOwnerConf.read("serviceOwner.json").nodeUrl
        ).getNetworkType
      )
    ) {}
