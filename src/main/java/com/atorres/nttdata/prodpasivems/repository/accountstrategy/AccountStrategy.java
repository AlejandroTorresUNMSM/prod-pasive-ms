package com.atorres.nttdata.prodpasivems.repository.accountstrategy;

import com.atorres.nttdata.prodpasivems.model.creditms.CreditDao;
import com.atorres.nttdata.prodpasivems.model.dto.AccountDto;
import com.atorres.nttdata.prodpasivems.utils.AccountCategory;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface AccountStrategy {
    Mono<Boolean> verifyAccount(Flux<AccountDto> listAccount);
    Mono<Boolean> verifyClient(Flux<AccountDto> listaAccount, Mono<AccountCategory> accountCategory, Flux<CreditDao> listCredit);
}
