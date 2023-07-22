package com.atorres.nttdata.prodpasivems.repository.accountstrategy;

import com.atorres.nttdata.prodpasivems.exception.CustomException;
import com.atorres.nttdata.prodpasivems.model.creditms.CreditDao;
import com.atorres.nttdata.prodpasivems.model.dao.AccountDao;
import com.atorres.nttdata.prodpasivems.model.dto.AccountDto;
import com.atorres.nttdata.prodpasivems.utils.AccountCategory;
import com.atorres.nttdata.prodpasivems.utils.AccountType;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.util.Pair;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Component
@Slf4j
public class PersonalAccountStrategy implements AccountStrategy{
    /**
     * Metodo que verifica si el cliente cumple como personal
     * @param listaAccount lista cuentas del cliente
     * @param accountCategory categoria de la cuenta ingresada
     * @param listCredit lista de creditos
     * @return boolean
     */
    @Override
    public Mono<Boolean> verifyClient(Flux<AccountDto> listaAccount, Mono<AccountCategory> accountCategory, Flux<CreditDao> listCredit){
        return accountCategory.filter(enumValue -> enumValue.equals(AccountCategory.VIP) || enumValue.equals(AccountCategory.NORMAL))
                .switchIfEmpty(Mono.error(new CustomException(HttpStatus.BAD_REQUEST,"Las cuentas personales no pueden ser MYPE")))
                .flatMap(enumValue -> enumValue.equals(AccountCategory.VIP)? verifyVip(listaAccount,listCredit) : Mono.just(true))
                .single()
                .flatMap(band -> Boolean.FALSE.equals(band) ? Mono.just(false) : verifyAccount(listaAccount))
                .doOnNext(value -> log.info("verifyClient: "+value.toString()));
    }

    /**
     * Metodo que verifica que las cuenta personal
     * @param listAccount lista cuentas
     * @return boolean
     */
    @Override
    public Mono<Boolean> verifyAccount(Flux<AccountDto> listAccount) {
        return listAccount
                .groupBy(AccountDto::getType)
                .flatMap(group -> group.count().map(count -> Pair.of(group.key(), count)))
                .collectList()
                .map(groups -> groups.size() <= 3 && !groups.isEmpty() && groups.stream().allMatch(pair -> pair.getSecond() == 1))
                .flatMap(value -> Boolean.TRUE.equals(value) ? Mono.just(true) : Mono.error(new CustomException(HttpStatus.BAD_REQUEST, "Cliente personal solo puede tener 1 cuenta de cada una")));
    }

    /**
     * Metodo que verifica si el cliente puede crear un producto vip , primero verifica si linea de credito
     * @param listAccount lista cuentas
     * @param listCredit lista creditos
     * @return boolean
     */
    public Mono<Boolean> verifyVip(Flux<AccountDto> listAccount,Flux<CreditDao> listCredit){
        return listCredit.any(credit -> true)
                .switchIfEmpty(Mono.error(new CustomException(HttpStatus.BAD_REQUEST,"El cliente VIP debe tener al menos un credito")))
                .flatMap(ac -> Boolean.FALSE.equals(ac) ? this.verifyVipAccount(listAccount) : Mono.just(false))
                .doOnNext(value -> log.info("verifyVip: "+value.toString()));
    }

    /**
     * Metodo que verifica si sus cuentas califican para un producto vip
     * @param listAccount lista cuentas
     * @return boolean
     */
    public Mono<Boolean> verifyVipAccount(Flux<AccountDto> listAccount) {
        return listAccount
                .filter(account -> account.getBalance().doubleValue() >=500 && account.getType().equals(AccountType.CA))
                .switchIfEmpty(Mono.error(new CustomException(HttpStatus.BAD_REQUEST,"El cliente VIP debe tener una cuenta CA con minimo 500 soles")))
                .doOnNext(value -> log.info("verifyVipAccount: "+value.toString()))
                .hasElements();
    }


}
