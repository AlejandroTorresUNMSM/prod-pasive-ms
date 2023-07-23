package com.atorres.nttdata.prodpasivems.repository.accountstrategy;

import com.atorres.nttdata.prodpasivems.exception.CustomException;
import com.atorres.nttdata.prodpasivems.model.creditms.CreditDto;
import com.atorres.nttdata.prodpasivems.model.dto.AccountDto;
import com.atorres.nttdata.prodpasivems.utils.AccountCategory;
import com.atorres.nttdata.prodpasivems.utils.AccountType;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Component
public class BussinesAccountStrategy implements  AccountStrategy{
    /**
     * Metodo que verifica si el cliente cumple como bussines
     * @param listaAccount lista cuentas
     * @param accountCategory categoria cuenta ingresada
     * @param listCredit lista creditos
     * @return boolean
     */
    @Override
    public Mono<Boolean> verifyClient(Flux<AccountDto> listaAccount, Mono<AccountCategory> accountCategory, Flux<CreditDto> listCredit){
        return accountCategory.filter(enumValue -> enumValue.equals(AccountCategory.MYPE) || enumValue.equals(AccountCategory.NORMAL))
                .switchIfEmpty(Mono.error(new CustomException(HttpStatus.BAD_REQUEST,"Las cuentas bussines no pueden ser VIP")))
                .flatMap(enumValue -> enumValue.equals(AccountCategory.MYPE)? verifyMype(listaAccount,listCredit) : Mono.just(true))
                .single()
                .flatMap(band -> Boolean.FALSE.equals(band) ? Mono.just(false) : verifyAccount(listaAccount));
    }

    /**
     * Metodo que verifica las cuentas bussines
     * @param listAccount lista cuentas
     * @return boolean
     */
    @Override
    public Mono<Boolean> verifyAccount(Flux<AccountDto> listAccount) {
        return listAccount
                .all(product -> product.getType().equals(AccountType.CC) && product.getBalance().doubleValue()>=0)
                .flatMap(exist -> exist.equals(Boolean.FALSE) ? Mono.error(new CustomException(HttpStatus.BAD_REQUEST, "Los clientes bussines solo pueden tener cuentas CC")) : Mono.just(Boolean.TRUE));
    }

    /**
     * Metodo que verifica si cumple para un producto mype
     * @param listAccount lista cuentas
     * @param listCredit lisa creditos
     * @return boolean
     */
    public Mono<Boolean> verifyMype(Flux<AccountDto> listAccount,Flux<CreditDto> listCredit) {
        return listCredit.any(credit -> true)
                .flatMap(ac -> Boolean.TRUE.equals(ac) ? verifyMypeAccount(listAccount) : Mono.error(new CustomException(HttpStatus.BAD_REQUEST,"El cliente MYPE tiene que tener al menos un credito")));
    }

    /**
     * Metodo que verifica si las cuentas califican para un producto mype
     * @param listAccount lista cuentas
     * @return boolean
     */
    public Mono<Boolean> verifyMypeAccount(Flux<AccountDto> listAccount){
        return listAccount
                .filter(account -> account.getAccountCategory().equals(AccountCategory.NORMAL) && account.getType().equals(AccountType.CC))
                .switchIfEmpty(Mono.error(new CustomException(HttpStatus.BAD_REQUEST,"El cliente MYPE tiene que tener al menos una cuenta CC NORMAL")))
                .hasElements();
    }
}
