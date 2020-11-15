# request

HTTP client for haskll, inpired by [requests](https://requests.readthedocs.io/) and [http-dispatch](https://github.com/owainlewis/http-dispatch).

## Usage

```haskell
import Network.HTTP.Request

let req = Request GET "https://api.leancloud.cn/1.1/date" [] Nothing
res <- send req
```

## About the Project

Vox is &copy; 2016-2020 by [aisk](https://github.com/aisk).

### License

Vox is distributed by a [BSD license](https://github.com/aisk/request/tree/master/LICENSE).
