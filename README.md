
# Lilium Scala API

Frontend Sends data about nft collection. This API creates necessary contracts and uploads AVL data to the database.


## Deployment

- Make sure your serviceOwner.json file is filled in with proper details

```bash
 docker-compose up -d --build
```


## Documentation

- Two endpoints currently
  - localhost:9000/submitCollection
    - This is a post request that accepts json of the [this format](https://github.com/LiliumErgo/scala-api/blob/main/dataFormat/finalFormat.json)
    - This will generate contracts and avl trees
    - The avl trees will be stored in a database
  - localhost:9000/generateCollectionIssuerHex 
    - This is a post request that accepts json of collectionInfo and socials. It returns the encoded version of those values.

